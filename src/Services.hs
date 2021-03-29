{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Services where

import ServerDB
import ServerMessages
import ClientMessages
import Channels
import Control.Lens
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)
import ServerDB
import Database.Persist.Sqlite
import Data.Maybe
import Data.Aeson

authenticationService :: ServerChans -> IO ()
authenticationService chans = forever $ do
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
          let status = Status $ if isKey then "Ok" else "User already in db"
          print status
          writeChan (conn^.inChan) status
        _ -> do 
          putStrLn "-- Not impl --"
          return ()
      putStrLn "-- End receive connect message--"
      writeChan (conn^.inChan) (Status "Ok")
    _ -> return ()

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = forever $ threadDelay 10000000

createLobby :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createLobby chans (client1, client2) questions = undefined
