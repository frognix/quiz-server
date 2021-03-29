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
import Control.Concurrent (threadDelay, newMVar, putMVar, newEmptyMVar)
import ServerDB
import Database.Persist.Sqlite
import Data.Maybe
import Data.Aeson
import Data.Text(Text)

authenticationService :: ServerChans -> IO ()
authenticationService chans = forever $ do
  connectedUser <- newEmptyMVar 
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
            user <- selectFirst [UserUsername ==. login, UserAdmin ==. False , UserPassword ==. password] []
            return $ isJust user
          let status = Status $ if isUser then Ok else NotFound
          putMVar connectedUser [login] 
          print status
          writeChan (conn^.inChan) status
        _ -> do 
          putStrLn "-- Not impl --"
          return ()
      putStrLn "-- End receive connect message--"
      writeChan (conn^.inChan) $ Status Ok
    _ -> return ()

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = forever $ threadDelay 10000000

createLobby :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createLobby chans (client1, client2) questions = undefined
