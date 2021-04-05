{-# LANGUAGE OverloadedStrings #-}
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

type Users = [Text]

type AuthenticationServiceState = StateT Users ServerWorker

authenticationService :: ServerWorker ()
authenticationService = flip evalStateT [] $ do
  lift $ putLog "Start authentication service"
  forever $ do
    client <- lift fromAuth
    lift . putLog $ "Authentication: new client"
    case client of
      ConnectMsg conn -> do
        msg <- liftIO $ readMsg conn
        lift . putLog $ "Authentication: receive message: " ++ show msg
        handleUserMessage client conn msg
      DisconnectMsg client -> do
        lift . putLog $ "Authentication: client disconnected: " ++ show (client^.user.to userUsername)
        modify $ filter (/=client^.user.to userUsername)

handleUserMessage :: Connection -> ClientChan -> UserMessage -> AuthenticationServiceState ()
handleUserMessage connection clientChan (Registration login password) = do
  maybeKey <- lift . withDB $ insertUnique $ User login password False
  lift $ toAuth connection
  liftIO $ writeMsg clientChan $ Status $ if isJust maybeKey then Ok else AlreadyInDb
handleUserMessage connection clientChan (Authorization login password) = do
  maybeUser <- lift . withDB $ selectFirst [UserUsername ==. login, UserPassword ==. password] []
  status <- withMaybe maybeUser (lift $ toAuth connection >> return (Status NotFound)) $ \(Entity _ user) -> do
    userConnected <- gets (elem login)
    if userConnected then do
      lift $ toAuth connection
      return $ Status AlreadyConnected
    else do
      modify (login :)
      lift . toLobby $ Client user clientChan
      return $ Status Ok
  liftIO $ writeMsg clientChan status
handleUserMessage _ clientChan _ = liftIO $ writeMsg clientChan $ Status UnexpectedMessageType
