{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
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
import Data.Maybe 
import Database.Persist.Sqlite (runSqlite)
import Database.Persist (PersistUniqueWrite(insertUnique))


authenticationService :: ServerChans -> IO ()
authenticationService chans = forever $ do
  putStrLn "Start authentication"
  client <- readChan $ chans^.authChan
  print "Client message"
  case client of
    ConnectMsg conn -> do
      putStrLn "--Receive connect message--"
      msg <- readChan $ conn^.outChan
      case msg of 
        Registration login password -> do 
          isKey <- runSqlite dataBaseAddress $ do
            key <- insertUnique $ User login password False 
            return $ isJust key
          let status = Status $ if isKey then "Ok" else "User already in database"
          print status
        _ -> do 
          return ()
      putStrLn "-- End receive connect message--"
    _ -> return ()

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = return () -- TODO

createLobby :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createLobby chans (client1, client2) questions = undefined
