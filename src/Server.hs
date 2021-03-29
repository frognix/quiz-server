{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Lens

import ServerMessages
import ClientMessages
import ServerDB
import Services
import Channels
import Admin

import qualified Network.WebSockets as WS
import Data.Aeson (decode,encode)
import Control.Monad.Extra (whenJust)
import Control.Monad
import Control.Exception (finally,try)
import Control.Concurrent.Chan
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (race,async,runConcurrently, Concurrently (runConcurrently, Concurrently))
import Control.Applicative

runServer :: IO ()
runServer = do
  initDB
  -- fillTables
  lobbyManagerChan <- newChan
  authenticationChan <- newChan
  let chans = ServerChans authenticationChan lobbyManagerChan
  res <- runConcurrently $ foldl1 (<|>) $ map Concurrently [
      authenticationService chans,
      lobbyManagerService chans,
      WS.runServer "127.0.0.1" 8080 $ clientHandler chans
    ]
  return ()

clientHandler :: ServerChans -> WS.ServerApp
clientHandler chans pending = do
  conn <- WS.acceptRequest pending
  let path = WS.requestPath $ WS.pendingRequest pending
  case path of
    "/admin" -> do
          createAdmin conn
    _        ->  do
          clientChan <- newChan
          serverChan <- newChan
          let channels = ClientChan serverChan clientChan
          writeChan (chans^.authChan) $ ConnectMsg channels
          createClient channels conn

createClient :: ClientChan -> WS.Connection -> IO ()
createClient chans conn = flip finally disconnect $ WS.withPingThread conn 30 (return ()) $ forever $ do
  result <- race (readChan $ chans^.inChan) (decode <$> WS.receiveData conn)
  case result of
    Left  msg -> WS.sendTextData conn $ encode msg
    Right msg -> whenJust msg $ writeChan $ chans^.outChan
  where disconnect = do
          writeChan (chans^.outChan) Disconnect
          id <- myThreadId
          putStrLn $ "Client " ++ show id ++ " disconnected"
