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
import Control.Exception (finally)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Concurrent.Async (race)

runServer :: IO ()
runServer = do
  initDB
  -- fillTables
  lobbyManagerChan <- newChan
  authenticationChan <- newChan
  let chans = ServerChans authenticationChan lobbyManagerChan
  forkIO $ authenticationService chans
  forkIO $ lobbyManagerService chans
  WS.runServer "127.0.0.1" 8080 $ clientHandler chans

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
