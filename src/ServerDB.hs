{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
module ServerDB where
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text,unpack)
import GHC.Generics

import System.Directory
import Control.Monad (when)

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)

withDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
withDB = runSqlite dataBaseAddress

dataBaseAddress = "./database/quiz-database.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    password Text
    admin Bool
    UniqueUsername username
    deriving Show
Topic
    title Text
    info Text
    UniqueTitle title
    deriving Show Generic Eq
Question
    text Text
    topicId TopicId
    correctAnswer Int
    answers [Text]
    deriving Show
|]

initDB :: IO ()
initDB = withDB $ runMigration migrateAll

deleteDB :: IO ()
deleteDB = do
  fileExists <- doesFileExist $ unpack dataBaseAddress
  when fileExists $ removeFile $ unpack dataBaseAddress

fillTables :: IO ()
fillTables = withDB $ do
  insertUnique $ User "admin" "admin" True
  insertUnique $ User "user" "12345" True

  progId <- insertUnique $ Topic "Programming" "Questions about programming"
  case progId of
    Nothing -> return ()
    Just id -> do
      insert $ Question "What is C++" id 1 ["Programming language",
                                            "The best programming language in the world",
                                            "fish",
                                            "Just three letters"]
      return ()
