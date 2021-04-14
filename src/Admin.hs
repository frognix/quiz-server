{-# LANGUAGE OverloadedStrings #-}
module Admin (createAdmin) where

import AdminMessages
import Database hiding (get)
import Extra.Tools
import Control.Lens
import qualified Network.WebSockets as WS
import Data.Aeson (decode, encode)
import Control.Monad
import Control.Monad.State

import ServerWorker

createAdmin :: WS.Connection -> ServerWorker ()
createAdmin conn = flip evalStateT False $ websocketThread conn onCreate onDestroy $ do
  maybeMsg <- decode <$> liftIO (WS.receiveData conn)
  withMaybe maybeMsg (liftIO $ WS.sendTextData conn . encode $ Status BadMessageStructure) $ \msg -> do
    lift . putLog $ "Admin: message: " ++ show msg
    adminAuthorized <- get
    if adminAuthorized then do
      serverAnswser <- lift $ adminAction msg
      liftIO $ WS.sendTextData conn $ encode serverAnswser
    else do
     isAdmin <- lift $ withDB $ exists [
       UserUsername ==. login msg,
       UserAdmin    ==. True,
       UserPassword ==. password msg]
     put isAdmin
     let status = Status $ if isAdmin then Ok else NotFound
     liftIO $ WS.sendTextData conn . encode $ status
  where onCreate  = lift $ putLog "Admin thread created"
        onDestroy = lift $ putLog "Admin thread destroyed"

toAdminTopic :: Topic -> [Question] -> AdminTopic
toAdminTopic (Topic title info) questions = AdminTopic title info $ map toAdminQuestion questions

toAdminQuestion :: Question -> AdminQuestion
toAdminQuestion (Question text _ answer answers) = AdminQuestion text answer answers

adminAction :: AdminMessage -> ServerWorker AdminServerMessage
adminAction GetTopicList = do
  adminTopics <- withDB $ do
    topics <- selectList [] []
    forM topics $ \(Entity key topic) -> do
      questions <- selectList [QuestionTopicId ==. key] []
      return $ toAdminTopic topic (map entityVal questions)
  putLog $ show $ TopicList adminTopics
  return $ TopicList adminTopics
adminAction (DeleteTopic title) = withDB $ do
  topic <- entityKey <$$> getBy (UniqueTitle title)
  withMaybe topic (return $ Status NotFound) $ \key -> do
    deleteWhere [QuestionTopicId ==. key]
    delete key
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
