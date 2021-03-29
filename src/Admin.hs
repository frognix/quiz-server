{-# LANGUAGE OverloadedStrings #-}
module Admin (createAdmin) where

import AdminMessages
import Channels
import ServerDB
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
createAdmin conn = flip finally disconnect $ WS.withPingThread conn 30 (return ()) $ forever $ do
  msg <- decode <$> WS.receiveData conn
  case msg of
    Just (Authorization login password) -> do
      isAdmin <- runSqlite dataBaseAddress $ do
        admin <- selectFirst [UserUsername ==. login, UserAdmin ==. True, UserPassword ==. password] []
        return $ isJust admin
      let status = Status $ if isAdmin then "Ok" else "Wrong username or password"
      WS.sendTextData conn . encode $ status
      guard isAdmin
      putStrLn $ "Admin authorized: " ++ unpack login
      authorizedAdmin conn
    _ -> return ()
  where disconnect = do
          putStrLn "Admin disconnected"

authorizedAdmin :: WS.Connection -> IO ()
authorizedAdmin conn = forever $ do
  maybeMessage <- decode <$> WS.receiveData conn
  case maybeMessage of
    Just message -> do
      serverAnwser <- adminAction message
      WS.sendTextData conn $ encode serverAnwser
    Nothing      -> return ()

toAdminTopic :: Topic -> [Question] -> AdminTopic
toAdminTopic (Topic title info) questions = AdminTopic title info $ map toAdminQuestion questions

toAdminQuestion :: Question -> AdminQuestion
toAdminQuestion (Question text _ answer answers) = AdminQuestion text answer answers

maybeIf :: Maybe a -> b -> (a -> b) -> b
maybeIf (Just v) _ func = func v
maybeIf _ value _       = value

adminAction :: AdminMessage -> IO AdminServerMessage
adminAction GetTopicList = runSqlite dataBaseAddress $ do
  topics <- selectList [] []
  adminTopics <- forM topics $ \(Entity key topic) -> do
    questions <- selectList [QuestionTopicId ==. key] []
    return $ toAdminTopic topic (map entityVal questions)
  return $ TopicList adminTopics
adminAction (DeleteTopic title) = runSqlite dataBaseAddress $ do
  topic <- (fmap . fmap) entityKey . getBy $ UniqueTitle title
  maybeIf topic (return $ Status "Topic not found") $ \key -> do
    delete key
    deleteWhere [QuestionTopicId ==. key]
    return $ Status "Ok"
adminAction (EditTopic (AdminTopic title info questions)) = runSqlite dataBaseAddress $ do
  maybeTopic <- (fmap . fmap) entityKey . getBy $ UniqueTitle title
  topic <- maybeIf maybeTopic (insert $ Topic title info) (return . return . fromJust $ maybeTopic)
  update topic [TopicInfo =. info]
  deleteWhere [QuestionTopicId ==. topic]
  forM_ questions $ \(AdminQuestion text answer answers) -> do
    insert $ Question text topic answer answers
  return (Status "Ok")
adminAction _ = return (Status "Unexpected message type")
