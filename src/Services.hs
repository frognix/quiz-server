{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Services where

import ServerDB
import ServerMessages
import ClientMessages
import Channels
import Extra.Tools ( StatusType(NotFound, AlreadyInDb, Ok, UnexpectedMessageType), withMaybe)
import Client (readMsg,writeMsg,ClientChan)
import Control.Lens
import qualified Data.Text.IO as TextIO
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, newMVar, putMVar, newEmptyMVar, modifyMVar_, withMVar, readMVar, tryReadMVar)
import Database.Persist.Sqlite
import Data.Maybe
import Data.Text.IO as T
import Data.Text (pack, Text)
import Control.Concurrent.Extra (MVar)
import Data.List

authenticationService :: ServerChans -> IO ()
authenticationService chans = do
  T.putStrLn "Start authentication service"
  connectedUsers <- newMVar []
  forever $ do
    client <- readChan $ chans^.authChan
    case client of
      ConnectMsg conn -> do
        msg <- readMsg conn
        handleUserMessage connectedUsers client conn msg
      DisconnectMsg client -> do
        modifyMVar_ connectedUsers $ deleteFromListIO . userUsername $ client^.user
        return ()
  where
    handleUserMessage :: MVar [Text] -> Connection -> ClientChan -> UserMessage -> IO ()
    handleUserMessage _ connection clientChan (Registration login password) = do
      maybeKey <- withDB $ insertUnique $ User login password False
      when (isNothing maybeKey) $ writeChan (chans^.authChan) connection
      writeMsg clientChan $ Status $ if isJust maybeKey then Ok else AlreadyInDb

    handleUserMessage connectedUsers connection clientChan (Authorization login password) = do
      maybeUser <- withDB $ selectFirst [UserUsername ==. login, UserPassword ==. password] []
      withMaybe maybeUser (writeChan (chans^.authChan) connection) $ \(Entity _ user) -> do
        writeChan (chans^.lobbyChan) $ Client user clientChan
        isUserExist <- findMVar login connectedUsers
        when isUserExist $ modifyMVar_ connectedUsers $ addToListIO login
      writeMsg clientChan $ Status $ if isJust maybeUser then Ok else NotFound

    handleUserMessage _ _ clientChan _ = writeMsg clientChan $ Status UnexpectedMessageType

addToListIO :: a -> [a] -> IO [a]
addToListIO x xs = return $ x : xs

deleteFromListIO :: Eq a => a -> [a] -> IO [a]
deleteFromListIO x xs = return $ filter (/= x) xs

findMVar :: Eq a => a -> MVar [a] -> IO Bool
findMVar x mvar = do
  xs <- readMVar mvar
  return $ isNothing $ find (== x) xs
