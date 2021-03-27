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
module DataBase where
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    login Text
    password Text
    deriving Show
Topic
    title Text
    info Text
    deriving Show
Question
    text Text
    topicId TopicId
    correctAnswer Int
    answers [Text]
    deriving Show
|]

dataBaseAddress = "./database/quiz-database.sqlite"

findUser :: Text -> IO (Maybe (Entity User))
findUser login = runSqlite dataBaseAddress $ do
    user <- selectFirst [UserLogin ==. login] []
    return user

selectKey f s = do
  keys <- selectKeysList f s
  if length keys == 0 then return Nothing
    else return $ Just (head keys)

getQuestions :: Text -> IO (Maybe [Entity Question])
getQuestions topicTitle = runSqlite dataBaseAddress $ do
    topic <- selectKey [TopicTitle ==. topicTitle] []
    case topic of
        Just v -> do
            questions <- selectList [QuestionTopicId ==. v] []
            return $ Just questions
        Nothing -> return Nothing
