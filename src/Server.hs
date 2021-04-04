{-# LANGUAGE OverloadedStrings #-}
module Server (runServer,mkServerConfig,ServerAddress(..)) where

import AuthenticationService
import Database.Actions
import Channels
import Admin
import Extra.Tools
import LobbyManager
import Client (createClientThread, mkClientChan)

import qualified Network.WebSockets as WS
import Control.Concurrent.Chan
import Control.Concurrent.Async (runConcurrently, Concurrently (runConcurrently, Concurrently))
import Control.Applicative

import ServerWorker

runServer :: ServerConfig -> IO ()
runServer = runServerWorker server

server :: ServerWorker ()
server = do
  initDB
  config <- ask
  liftIO $ runConcurrently $ foldl1 (<|>) $ map (\act -> Concurrently $ runServerWorker act config) [
      authenticationService,
      lobbyManagerService,
      runWebServer clientHandler
    ]
  return ()

runWebServer :: (WS.PendingConnection -> ServerWorker ()) -> ServerWorker ()
runWebServer func = do
  config <- ask
  ServerAddress ip port <- askAddress
  let func' = (\conn -> runServerWorker (func conn) config)
  liftIO $ WS.runServer ip port func'

clientHandler :: WS.PendingConnection -> ServerWorker ()
clientHandler pending = do
  conn <- liftIO $ WS.acceptRequest pending
  withGuard (url == "/admin") $ createAdmin conn
  clientChan <- liftIO newChan
  serverChan <- liftIO newChan
  let channels = mkClientChan serverChan clientChan
  toAuth $ ConnectMsg channels
  createClientThread channels conn
  where url = WS.requestPath $ WS.pendingRequest pending
