{-# LANGUAGE OverloadedStrings #-}
module Server where

import Control.Lens

import ServerMessages
import ClientMessages
import ServerDB
import Services
import Channels
import Admin
import Extra.Tools
import LobbyManager
import Client (createClientThread, mkClientChan)

import qualified Network.WebSockets as WS
import Data.Aeson (decode,encode)
import Control.Monad.Extra (whenJust)
import Control.Monad
import Control.Exception (finally,try)
import Control.Concurrent.Chan
import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (race,async,runConcurrently, Concurrently (runConcurrently, Concurrently))
import Control.Applicative

runServer :: IO ()
runServer = do
  initDB
  -- fillTables
  lobbyManagerChan <- newChan
  authenticationChan <- newChan
  let chans = ServerChans authenticationChan lobbyManagerChan
  _ <- runConcurrently $ foldl1 (<|>) $ map Concurrently [
      authenticationService chans,
      lobbyManagerService chans,
      WS.runServer "127.0.0.1" 8080 $ clientHandler chans
    ]
  return ()

clientHandler :: ServerChans -> WS.ServerApp
clientHandler chans pending = do
  conn <- WS.acceptRequest pending
  withGuard (url == "/admin") $ createAdmin conn
  clientChan <- newChan
  serverChan <- newChan
  let channels = mkClientChan serverChan clientChan
  writeChan (chans^.authChan) $ ConnectMsg channels
  createClientThread channels conn
  where url = WS.requestPath $ WS.pendingRequest pending
