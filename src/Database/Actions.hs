{-# LANGUAGE OverloadedStrings #-}
module Database.Actions where

import Database
import Extra.Tools
import System.Directory
import ServerWorker
import Data.Text
import Control.Monad

initDB :: ServerWorker ()
initDB = withDB $ runMigration migrateAll

deleteDB :: ServerWorker ()
deleteDB = do
  dataBaseAddress <- askDatabase
  fileExists <- liftIO $ doesFileExist $ unpack dataBaseAddress
  liftIO $ when fileExists $ removeFile $ unpack dataBaseAddress

fillTables :: ServerWorker ()
fillTables = withDB $ do
  _ <- insertUnique $ User "admin" "admin" True
  _ <- insertUnique $ User "user"  "12345" False
  maybeId <- insertUnique $ Topic "Programming" "Questions about programming"
  case maybeId of
    Nothing -> return ()
    Just topic -> do
      insert_ $ Question "What is C++" topic SecondAnswer ["Programming language",
                                            "The best programming language in the world",
                                            "fish",
                                            "Just three letters"]
      return ()
