{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Services where

import ServerDB
import ServerMessages
import ClientMessages
import Channels
import ExtraTools
import Control.Lens
import qualified Data.Text.IO as TextIO
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, newMVar, putMVar, newEmptyMVar, modifyMVar_, withMVar, readMVar, tryReadMVar)
import Database.Persist.Sqlite
import Data.Maybe

authenticationService :: ServerChans -> IO ()
authenticationService chans = do 
  connectedUsers <- newMVar []
  forever $ do 
    putStrLn "Start authentication"
    client <- readChan $ chans^.authChan
    print "Client message"
    case client of
      ConnectMsg conn -> do
        putStrLn "--Receive connect message--"
        msg <- readChan $ conn^.outChan
        print msg
        case msg of 
          Registration login password -> do 
            isKey <- runSqlite dataBaseAddress $ do 
              key <- insertUnique $ User login password False
              return $ isJust key
            let status = Status $ if isKey then Ok else AlreadyInDb
            print status
            writeChan (conn^.inChan) status
          Authorization login password -> do 
            isUser <- runSqlite dataBaseAddress $ do 
              user <- selectFirst [UserUsername ==. login, UserPassword ==. password] []
              return $ isJust user 
            when isUser $ modifyMVar_ connectedUsers $ addToListIO login
            when isUser $ writeChan (chans^.lobbyChan) $ Client (User login password False) conn     
            let status = Status $ if isUser then Ok else NotFound   
            print status
            writeChan (conn^.inChan) status
          _ -> do 
            putStrLn "-- Not impl --"
            return ()
        putStrLn "-- End receive connect message--"
      DisconnectMsg client -> do
        modifyMVar_ connectedUsers $ deleteFromListIO . userUsername $ client^.user 
        return ()

addToListIO :: a -> [a] -> IO [a] 
addToListIO x xs = do return $ x : xs

deleteFromListIO :: Eq a => a -> [a] -> IO [a]
deleteFromListIO x xs = do return $ filter (/= x) xs

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = forever $ threadDelay 10000000

createLobby :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createLobby chans (client1, client2) questions = undefined
