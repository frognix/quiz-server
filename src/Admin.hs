{-# LANGUAGE OverloadedStrings #-}
module Admin (createAdmin) where

import AdminMessages
import Channels
import ServerDB
import Extra.Tools
import Control.Lens
import qualified Network.WebSockets as WS
import Data.Aeson (decode, encode)
import Control.Monad.Extra (whenJust)
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan
import Database.Persist hiding (get)
import Database.Persist.TH
import Data.Text (Text, unpack)
import Data.Maybe
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.State

createAdmin :: WS.Connection -> IO ()
createAdmin conn = websocketThread conn onCreate onDestroy $ flip evalStateT False $ do
  maybeMsg <- decode <$> liftIO (WS.receiveData conn)
  withMaybe maybeMsg (liftIO $ WS.sendTextData conn . encode $ Status BadMessageStructure) $ \msg -> do
    adminAuthorized <- get
    case adminAuthorized of
      True  -> do
        serverAnswser <- liftIO $ adminAction msg
        liftIO $ WS.sendTextData conn $ encode serverAnswser
      False -> do
        isAdmin <- liftIO $ withDB $ exists [
          UserUsername ==. msg^.login,
          UserAdmin    ==. True,
          UserPassword ==. msg^.password]
        put isAdmin
        let status = Status $ if isAdmin then Ok else NotFound
        liftIO $ WS.sendTextData conn . encode $ status
  where onCreate  = return ()
        onDestroy = return ()

toAdminTopic :: Topic -> [Question] -> AdminTopic
toAdminTopic (Topic title info) questions = AdminTopic title info $ map toAdminQuestion questions

toAdminQuestion :: Question -> AdminQuestion
toAdminQuestion (Question text _ answer answers) = AdminQuestion text answer answers

adminAction :: AdminMessage -> IO AdminServerMessage
adminAction GetTopicList = withDB $ do
  topics <- selectList [] []
  adminTopics <- forM topics $ \(Entity key topic) -> do
    questions <- selectList [QuestionTopicId ==. key] []
    return $ toAdminTopic topic (map entityVal questions)
  return $ TopicList adminTopics
adminAction (DeleteTopic title) = withDB $ do
  topic <- entityKey <$$> getBy (UniqueTitle title)
  withMaybe topic (return $ Status NotFound) $ \key -> do
    delete key
    deleteWhere [QuestionTopicId ==. key]
    return $ Status Ok
adminAction (EditTopic (AdminTopic title info questions)) = withDB $ do
  maybeTopic <- entityKey <$$> getBy (UniqueTitle title)
  topic <- withMaybe maybeTopic (insert $ Topic title info) return
  update topic [TopicInfo =. info]
  deleteWhere  [QuestionTopicId ==. topic]
  forM_ questions $ \(AdminQuestion text answer answers) -> do
    insert $ Question text topic answer answers
  return $ Status Ok
adminAction _ = return $ Status UnexpectedMessageType
