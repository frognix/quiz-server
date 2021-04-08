{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module AuthenticationService where

import Database
import ServerMessages
import ClientMessages
import Channels
import Extra.Tools
import Client (readMsg,writeMsg)
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.List
import Control.Monad.State
import ServerWorker
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

type ClientWorker = Async (UserMessage, ClientChan)

newClientWorker :: ClientChan -> IO ClientWorker
newClientWorker chan = linkAsync $ do
  msg <- readMsg chan
  return (msg, chan)

data AuthenticationServiceState = AuthenticationServiceState { _clients :: [ClientWorker],  _authorizedClients :: [Text] }

makeLenses ''AuthenticationServiceState

type AuthenticationService = StateT AuthenticationServiceState ServerWorker

waitClients :: AuthenticationService (UserMessage, ClientChan)
waitClients = do
  clients' <- use clients
  liftIO $ when (null clients') . forever $ threadDelay 1000000
  (thread, responce@(msg, client)) <- liftIO $ waitAny clients'
  zoom clients $ modify $ filter (/=thread)
  return responce

waitNewClient :: AuthenticationService Connection
waitNewClient = lift fromAuth

addClient :: ClientChan -> AuthenticationService ()
addClient chan = do
  worker <- liftIO $ newClientWorker chan
  zoom clients $ modify (worker :)

authenticationService :: ServerWorker ()
authenticationService = flip evalStateT (AuthenticationServiceState [] []) $ do
  lift $ putLog "Start authentication service"
  forever $ do
    msg <- workerRace waitNewClient waitClients
    case msg of
      Left client -> do
        lift . putLog $ "Authentication: new client"
        case client of
          ConnectMsg conn -> addClient conn
          DisconnectMsg msg client -> do
            lift . putLog $ "Authentication: client disconnected: " ++ show (client^.user.to userUsername)
            zoom authorizedClients $ modify $ filter (/=client^.user.to userUsername)
            when (msg == LogOut) $ lift . toAuth . ConnectMsg $ client^.channels
      Right (msg, client) -> do
        lift . putLog $ "Authentication: new message: " ++ show msg
        handleUserMessage client msg

handleUserMessage :: ClientChan -> UserMessage -> AuthenticationService ()
handleUserMessage clientChan (Registration login password) = do
  maybeKey <- lift . withDB $ insertUnique $ User login password False
  lift $ toAuth $ ConnectMsg clientChan
  liftIO $ writeMsg clientChan $ Status $ if isJust maybeKey then Ok else AlreadyInDb
handleUserMessage clientChan (Authorization login password) = do
  maybeUser <- lift . withDB $ selectFirst [UserUsername ==. login, UserPassword ==. password] []
  status <- withMaybe maybeUser (lift $ toAuth (ConnectMsg clientChan) >> return (Status NotFound)) $ \(Entity _ user) -> do
    userConnected <- zoom authorizedClients $ gets (elem login)
    if userConnected then do
      lift $ toAuth $ ConnectMsg clientChan
      return $ Status AlreadyConnected
    else do
      zoom authorizedClients $ modify (login :)
      lift . toLobby $ Client user clientChan
      return $ Status Ok
  liftIO $ writeMsg clientChan status
handleUserMessage clientChan Disconnect = return ()
handleUserMessage clientChan _ = liftIO $ writeMsg clientChan $ Status UnexpectedMessageType
