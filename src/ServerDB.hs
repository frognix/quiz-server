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
import Data.Text (Text)

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
    deriving Show
Question
    text Text
    topicId TopicId
    correctAnswer Int
    answers [Text]
    deriving Show
|]

dataBaseAddress = "./database/quiz-database.sqlite"

initDB :: IO ()
initDB = runSqlite dataBaseAddress $ do
  runMigration migrateAll

fillTables :: IO ()
fillTables = runSqlite dataBaseAddress $ do
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

-- findUser :: Text -> IO (Maybe (Entity User))
-- findUser login = runSqlite dataBaseAddress $ do
--     selectFirst [UserUsername ==. login] []

-- selectKey f s = do
--   keys <- selectKeysList f s
--   if null keys then return Nothing
--     else return $ Just (head keys)

-- getQuestions :: Text -> IO (Maybe [Entity Question])
-- getQuestions topicTitle = runSqlite dataBaseAddress $ do
--     topic <- selectKey [TopicTitle ==. topicTitle] []
--     case topic of
--         Just v -> do
--             questions <- selectList [QuestionTopicId ==. v] []
--             return $ Just questions
--         Nothing -> return Nothing
