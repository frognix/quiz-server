{-# LANGUAGE OverloadedStrings #-}
module Admin (createAdmin) where

import AdminMessages
import Channels
import ServerDB
import ExtraTools
import Control.Lens
import qualified Network.WebSockets as WS
import Data.Aeson (decode, encode)
import Control.Monad.Extra (whenJust)
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text, unpack)
import Data.Maybe
import Control.Monad.IO.Class  (liftIO)

createAdmin :: WS.Connection -> IO ()
createAdmin conn =  websocketThread conn onCreate onDestroy $ do
  putStrLn "Admin created"
  maybeMsg <- decode <$> WS.receiveData conn
  withMaybe maybeMsg (return ()) $ \msg -> do
    isAdmin <- runSqlite dataBaseAddress $ exists [
      UserUsername ==. msg^.login,
      UserAdmin    ==. True,
      UserPassword ==. msg^.password]
    let status = Status $ if isAdmin then Ok else NotFound
    WS.sendTextData conn . encode $ status
    guard isAdmin
    putStrLn $ "Admin authorized: " ++ unpack (msg^.login)
    authorizedAdmin conn
  where onCreate  = putStrLn "Admin thread created"
        onDestroy = putStrLn "Admin thread destoyed"

authorizedAdmin :: WS.Connection -> IO ()
authorizedAdmin conn = forever $ do
  maybeMessage <- decode <$> WS.receiveData conn
  withMaybe maybeMessage (return ()) $ \message -> do
    serverAnwser <- adminAction message
    WS.sendTextData conn $ encode serverAnwser

toAdminTopic :: Topic -> [Question] -> AdminTopic
toAdminTopic (Topic title info) questions = AdminTopic title info $ map toAdminQuestion questions

toAdminQuestion :: Question -> AdminQuestion
toAdminQuestion (Question text _ answer answers) = AdminQuestion text answer answers

adminAction :: AdminMessage -> IO AdminServerMessage
adminAction GetTopicList = runSqlite dataBaseAddress $ do
  topics <- selectList [] []
  adminTopics <- forM topics $ \(Entity key topic) -> do
    questions <- selectList [QuestionTopicId ==. key] []
    return $ toAdminTopic topic (map entityVal questions)
  return $ TopicList adminTopics
adminAction (DeleteTopic title) = runSqlite dataBaseAddress $ do
  topic <- (fmap . fmap) entityKey . getBy $ UniqueTitle title
  withMaybe topic (return $ Status NotFound) $ \key -> do
    delete key
    deleteWhere [QuestionTopicId ==. key]
    return $ Status Ok
adminAction (EditTopic (AdminTopic title info questions)) = runSqlite dataBaseAddress $ do
  maybeTopic <- (fmap . fmap) entityKey . getBy $ UniqueTitle title
  topic <- withMaybe maybeTopic (insert $ Topic title info) return
  update topic [TopicInfo =. info]
  deleteWhere  [QuestionTopicId ==. topic]
  forM_ questions $ \(AdminQuestion text answer answers) -> do
    insert $ Question text topic answer answers
  return $ Status Ok
adminAction _ = return $ Status UnexpectedMessageType
