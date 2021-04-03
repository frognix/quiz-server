{-# LANGUAGE OverloadedStrings #-}
module AuthenticationService where

import Database
import ServerMessages
import ClientMessages
import Channels
import Extra.Tools ( StatusType(NotFound, AlreadyInDb, Ok, UnexpectedMessageType), withMaybe)
import Client (readMsg,writeMsg)
import Control.Lens
import Control.Monad
import Control.Concurrent (newMVar, modifyMVar_, readMVar)
import Data.Maybe
import Data.Text (Text)
import Control.Concurrent.Extra (MVar)
import Data.List
import ServerWorker

authenticationService :: ServerWorker ()
authenticationService = do
  putLog "Start authentication service"
  connectedUsers <- liftIO $ newMVar []
  forever $ do
    client <- fromAuth
    case client of
      ConnectMsg conn -> do
        msg <- liftIO $ readMsg conn
        handleUserMessage connectedUsers client conn msg
      DisconnectMsg client -> do
        liftIO $ modifyMVar_ connectedUsers $ deleteFromListIO . userUsername $ client^.user

handleUserMessage :: MVar [Text] -> Connection -> ClientChan -> UserMessage -> ServerWorker ()
handleUserMessage _ connection clientChan (Registration login password) = do
  maybeKey <- withDB $ insertUnique $ User login password False
  toAuth connection
  liftIO $ writeMsg clientChan $ Status $ if isJust maybeKey then Ok else AlreadyInDb
handleUserMessage connectedUsers connection clientChan (Authorization login password) = do
  maybeUser <- withDB $ selectFirst [UserUsername ==. login, UserPassword ==. password] []
  withMaybe maybeUser (toAuth connection) $ \(Entity _ user) -> do
    toLobby $ Client user clientChan
    isUserExist <- liftIO $ findMVar login connectedUsers
    liftIO $ when isUserExist $ modifyMVar_ connectedUsers $ addToListIO login
  liftIO $ writeMsg clientChan $ Status $ if isJust maybeUser then Ok else NotFound
handleUserMessage _ _ clientChan _ = liftIO $ writeMsg clientChan $ Status UnexpectedMessageType

addToListIO :: a -> [a] -> IO [a]
addToListIO x xs = return $ x : xs

deleteFromListIO :: Eq a => a -> [a] -> IO [a]
deleteFromListIO x xs = return $ filter (/= x) xs

findMVar :: Eq a => a -> MVar [a] -> IO Bool
findMVar x mvar = do
  xs <- readMVar mvar
  return $ isNothing $ find (== x) xs
