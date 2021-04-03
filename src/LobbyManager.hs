module LobbyManager (lobbyManagerService) where

import Channels
import Database hiding (get)
import ClientMessages
import ServerMessages hiding (topic,topics,status)
import Extra.Tools
import Extra.State
import PlayGround
import Client (readMsg,writeMsg)
import ServerWorker

import Control.Monad.State hiding (state)
import Control.Concurrent (threadDelay)
import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Concurrent.Async

-- | Message and client who sent it
type ClientResponce = (UserMessage, Client)

-- | Asyncronous action that wait for user responce
type ClientWaiter = Async ClientResponce

-- | State of lobby manager
type LobbyManagerState r = StateT ([ClientWaiter], Map.Map T.Text Client) ServerWorker r

-- | Part of lobby manager state with map of clients
-- who have already choosen a topic
type MapState r = StateT (Map.Map T.Text Client) ServerWorker r

-- | Part of lobby manager state with list of clients
-- who not have not yet choosen a topic
type ClientsState r = StateT [ClientWaiter] ServerWorker r

-- | Create new ClientWaiter from Client
clientWaiter :: Client -> IO ClientWaiter
clientWaiter client = linkAsync $ do
  msg <- client^.channels.to readMsg
  return (msg, client)

withMap :: MapState r -> LobbyManagerState r
withMap = withSnd

withClients :: ClientsState r -> LobbyManagerState r
withClients = withFst

-- | Waits all clients from list and returns
-- a responce from the first client to be completed,
-- then removes the client from the client list
waitClients :: LobbyManagerState ClientResponce
waitClients = do
  state <- withClients get
  liftIO $ when (null state) . forever $ threadDelay 1000000
  (thread, responce) <- liftIO $ waitAny state
  liftIO $ cancel thread
  withClients $ modify $ filter (/=thread)
  return responce

-- | Waits for a new client from LobbyManagerChan and adds it to
-- the client list
waitNewClient :: LobbyManagerState Client
waitNewClient = do
  client <- lift fromLobby
  waiter <- liftIO $ clientWaiter client
  withClients $ modify (waiter :)
  return client

-- | Waits for a client from LobbyManagerChan, then
-- waits for a message with a topic from the client,
-- if two clients send the same topic, then creates
-- a new playground
lobbyManagerService :: ServerWorker ()
lobbyManagerService = do
  flip evalStateT ([], Map.empty) $ forever $ do
    res <- workerRace waitClients waitNewClient
    case res of
      Left (message, client) -> do
        lobbyManagerAction client message
      Right client -> do
        topics <- lift $ entityVal <$$> withDB (selectList [] [])
        liftIO $ client^.channels.to writeMsg $ Topics topics

-- | Applies an action according to a message type
lobbyManagerAction :: Client -> UserMessage -> LobbyManagerState ()
lobbyManagerAction client Disconnect = lift . toAuth $ DisconnectMsg client
lobbyManagerAction client (SelectTopic topic) = do
  topicExists <- lift . withDB $ exists [TopicTitle ==. topic]
  let status = Status $ if topicExists then Ok else NotFound
  liftIO $ client^.channels.to writeMsg $ status
  when topicExists $ do
    maybeClient <- withMap $ gets (Map.lookup topic)
    withMaybe maybeClient (withMap $ modify $ Map.insert topic client) $ \client' -> do
      lift $ createPlayGround (client', client) []
      withMap $ modify $ Map.delete topic
lobbyManagerAction client _  = liftIO $ client^.channels.to writeMsg $ Status UnexpectedMessageType
