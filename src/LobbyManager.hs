module LobbyManager (lobbyManagerService) where

import Channels
import ServerDB
import ClientMessages
import ServerMessages
import ExtraTools
import PlayGround

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Control.Monad
import Database.Persist.Sqlite hiding (get)
import Control.Concurrent (threadDelay,myThreadId)
import Data.Map.Lens
import Control.Lens
import Control.Lens.At
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Concurrent.Chan

type ClientResponce = (UserMessage, Client)

type ClientWaiter = Async ClientResponce

type SubManagerChan = Chan (T.Text, Client)

clientWaiter :: Client -> IO ClientWaiter
clientWaiter client = async $ do
  msg <- client^.channels.outChan.to readChan
  return (msg, client)

waitClients :: [ClientWaiter] -> IO (ClientWaiter, ClientResponce)
waitClients = waitAny

addClient :: Client -> StateT [ClientWaiter] IO ()
addClient client = do
  waiter <- liftIO $ clientWaiter client
  modify (waiter :)

withDB = runSqlite dataBaseAddress

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = do
  subChan <- newChan
  topics <-  entityVal <$$> withDB (selectList [] [])
  subManager <- async $ lobbySubManager chans subChan
  flip evalStateT [] $ forever $ do
    list <- get
    res <- liftIO $ race newUser $ newTopic list
    case res of
      Left client -> do
        addClient client
        liftIO $ do
          putStrLn "New client"
          client^.channels.inChan.to writeChan $ Topics topics
      Right (thread, (message, client)) -> do
        modify $ filter (/=thread)
        liftIO $ do
          putStrLn $ "New message " ++ show message
          lobbyManagerAction chans subChan client message
  where newUser = chans^.lobbyChan.to readChan
        newTopic state = do
          if null state then forever $ threadDelay 1000000
          else liftIO $ waitClients state

lobbyManagerAction :: ServerChans -> SubManagerChan -> Client -> UserMessage -> IO ()
lobbyManagerAction chans _    client Disconnect          = chans^.authChan.to writeChan $ DisconnectMsg client
lobbyManagerAction chans chan client (SelectTopic topic) = do
  topicExists <- withDB $ exists [TopicTitle ==. topic]
  let status = Status $ if topicExists then Ok else NotFound
  client^.channels.inChan.to writeChan $ status
  if topicExists
    then writeChan chan (topic, client)
    else chans^.lobbyChan.to writeChan $ client
lobbyManagerAction _     _    client _                   = client^.channels.inChan.to writeChan $ Status UnexpectedMessageType

lobbySubManager :: ServerChans -> SubManagerChan -> IO ()
lobbySubManager chans chan = flip evalStateT Map.empty $ forever $ do
  (topic, client) <- liftIO $ readChan chan
  maybeClient <- gets (Map.lookup topic)
  withMaybe maybeClient (at topic ?= client) $ \client' -> do
    liftIO $ async $ createPlayGround chans (client', client) []
    modify $ Map.delete topic
  keys <- gets Map.keys
  liftIO $ print keys
