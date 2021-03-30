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
import Data.Text.IO as T
import Data.Text (pack)

authenticationService :: ServerChans -> IO ()
authenticationService chans = do 
  connectedUsers <- newMVar []
  forever $ flip mplus (return ()) $ do  
    T.putStrLn "Start authentication"
    client <- readChan $ chans^.authChan
    T.putStrLn "Client message"
    case client of
      ConnectMsg conn -> do
        T.putStrLn "--Receive connect message--"
        msg <- readChan $ conn^.outChan
        T.putStrLn $ pack $ show msg
        case msg of 
          Registration login password -> do 
            isKey <- runSqlite dataBaseAddress $ do 
              key <- insertUnique $ User login password False
              return $ isJust key
            let status = Status $ if isKey then Ok else AlreadyInDb
            T.putStrLn $ pack $ show status
            writeChan (conn^.inChan) status
          Authorization login password -> do 
            isUser <- runSqlite dataBaseAddress $ exists [UserUsername ==. login, UserPassword ==. password] 
            let status = Status $ if isUser then Ok else NotFound   
            T.putStrLn $ pack $ show status
            writeChan (conn^.inChan) status
            guard isUser 
            modifyMVar_ connectedUsers $ addToListIO login
            writeChan (chans^.lobbyChan) $ Client (User login password False) conn 
          _ -> do 
            T.putStrLn "-- Not impl --"
            return ()
        T.putStrLn "-- End receive connect message--"
      DisconnectMsg client -> do
        modifyMVar_ connectedUsers $ deleteFromListIO . userUsername $ client^.user 
        return ()

addToListIO :: a -> [a] -> IO [a] 
addToListIO x xs = return $ x : xs

deleteFromListIO :: Eq a => a -> [a] -> IO [a]
deleteFromListIO x xs = return $ filter (/= x) xs

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = forever $ threadDelay 10000000

createLobby :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createLobby chans (client1, client2) questions = undefined
