{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module ServerWorker (
  ServerWorker(),
  ServerAddress(..),
  ServerConfig,
  runServerWorker,
  websocketThread,
  putLog, workerRace,
  toAuth, fromAuth, toLobby, fromLobby,
  liftIO, ask, askDatabase, askAddress, mkServerConfig) where

import Control.Monad
import Control.Applicative

import Channels
import Extra.State (putReturn)

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.Async (race,async,cancel)
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Monad.Catch
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)
import Data.List.Extra (splitOn)
import Text.Read (readMaybe)

data ServerAddress = ServerAddress { ip :: String, port :: Int }

instance Show ServerAddress where
  show (ServerAddress ip port) = ip ++ ":" ++ show port

instance Read ServerAddress where
  readsPrec _ str = parse $ splitOn ":" str
    where parse :: [String] -> [(ServerAddress, String)]
          parse [addr, port] = maybe [] (\p -> [(ServerAddress addr p, "")]) (readMaybe port)
          parse _            = []

data ServerConfig = ServerConfig { address :: ServerAddress, dataBase :: Text, serverChans :: ServerChans, logs :: Bool }

mkServerConfig :: ServerAddress -> Text -> Bool -> IO ServerConfig
mkServerConfig addr db logs = do
  lobbyManagerChan   <- liftIO newChan
  authenticationChan <- liftIO newChan
  let chans = ServerChans authenticationChan lobbyManagerChan
  return $ ServerConfig addr db chans logs

newtype ServerWorker a = ServerWorker {
  runSW :: ReaderT ServerConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerConfig,
              Alternative, MonadPlus, MonadThrow, MonadCatch, MonadMask)

runServerWorker :: ServerWorker a -> ServerConfig -> IO a
runServerWorker worker = runReaderT (runSW worker)

putLog :: String -> ServerWorker ()
putLog message = do
  logs <- asks logs
  when logs $ liftIO $ putStrLn $ "Log: " ++ message

toAuth :: Connection -> ServerWorker ()
toAuth msg = do
  ServerChans auth _ <- asks serverChans
  liftIO $ writeChan auth msg

fromAuth :: ServerWorker Connection
fromAuth = do
  ServerChans auth _ <- asks serverChans
  liftIO $ readChan auth

toLobby :: Client -> ServerWorker ()
toLobby msg = do
  ServerChans _ lobby <- asks serverChans
  liftIO $ writeChan lobby msg

fromLobby :: ServerWorker Client
fromLobby = do
  ServerChans _ lobby <- asks serverChans
  liftIO $ readChan lobby

askDatabase :: ServerWorker Text
askDatabase = asks dataBase

askAddress :: ServerWorker ServerAddress
askAddress = asks address

websocketThread :: (Monad m, MonadIO m, MonadMask m) => WS.Connection -> m () -> m () -> m () -> m ()
websocketThread conn onCreate onDestroy work = do
  onCreate
  ping <- liftIO $ async $ forever $ do
    threadDelay (30*1000*1000)
    WS.sendPing conn ("Ping" :: Text)
  finally (forever work) $ onDestroy >> liftIO (cancel ping)

workerRace :: StateT s ServerWorker a -> StateT s ServerWorker b -> StateT s ServerWorker (Either a b)
workerRace first second = do
  state <- get
  config <- ask
  let left  = runServerWorker (runStateT first  state) config
      right = runServerWorker (runStateT second state) config
  res <- liftIO $ race left right
  putReturn res
