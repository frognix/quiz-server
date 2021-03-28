module Server where

import Control.Lens

import APIModels
import ServerDB
import Services

import qualified Network.WebSockets as WS
import Data.Aeson (decode)
import Control.Monad.Extra (whenJust)
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay, myThreadId)

runServer :: IO ()
runServer = do
  lobbyManagerChan <- newChan
  authenticationChan <- newChan
  let chans = ServerChans authenticationChan lobbyManagerChan
  forkIO $ authenticationService chans
  forkIO $ lobbyManagerService chans
  WS.runServer "127.0.0.1" 8080 $ clientHandler chans

clientHandler :: ServerChans -> WS.ServerApp
clientHandler chans pending = do
    conn <- WS.acceptRequest pending
    putStrLn "Client connected"
    clientChan <- newChan
    writeChan (chans^.authChan) $ ConnectMsg clientChan
    createClient clientChan conn

createClient :: ClientChan -> WS.Connection -> IO ()
createClient chan conn = flip finally disconnect $ WS.withPingThread conn 30 (return ()) $ forever $ do
  msg <- decode <$> WS.receiveData conn
  whenJust msg $ writeChan chan
  where disconnect = do
          writeChan chan Disconnect
          id <- myThreadId
          putStrLn $ "Client " ++ show id ++ " disconnected"
