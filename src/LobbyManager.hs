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
import Data.Maybe

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
type LobbyManagerState r = StateT (Map.Map T.Text ClientWaiter, [(T.Text, Client)]) ServerWorker r

-- | Part of lobby manager state with map of clients
-- who have already choosen a topic
type MapState r = StateT [(T.Text, Client)] ServerWorker r

-- | Part of lobby manager state with list of clients
-- who not have not yet choosen a topic
type ClientsState r = StateT (Map.Map T.Text ClientWaiter) ServerWorker r

-- | Create new ClientWaiter from Client
clientWaiter :: Client -> IO ClientWaiter
clientWaiter client = linkAsync $ do
  msg <- client^.channels.to readMsg
  return (msg, client)

withMap :: MapState r -> LobbyManagerState r
withMap = zoom _2

withClients :: ClientsState r -> LobbyManagerState r
withClients = zoom _1

removeClientThread :: Client -> LobbyManagerState ()
removeClientThread client = do
  maybeThread <- withClients . gets $ Map.lookup (client^.user.to userUsername)
  case maybeThread of
    Just thread -> do
      withClients . modify $ Map.delete (client^.user.to userUsername)
      liftIO $ cancel thread
    Nothing -> return ()

-- | Waits all clients from list and returns
-- a responce from the first client to be completed,
-- then removes the client from the client list
waitClients :: LobbyManagerState ClientResponce
waitClients = do
  state <- withClients get
  liftIO $ when (null state) . forever $ threadDelay 1000000
  (thread, responce@(msg, client)) <- liftIO $ waitAny $ Map.elems state
  withClients . modify $ Map.delete (client^.user.to userUsername)
  return responce

addClient :: Client -> LobbyManagerState ()
addClient client = do
  waiter <- liftIO $ clientWaiter client
  withClients $ modify $ Map.insert (client^.user.to userUsername) waiter

-- | Waits for a new client from LobbyManagerChan and adds it to
-- the client list
waitNewClient :: LobbyManagerState Client
waitNewClient = do
  client <- lift fromLobby
  addClient client
  return client

-- | Waits for a client from LobbyManagerChan, then
-- waits for a message with a topic from the client,
-- if two clients send the same topic, then creates
-- a new playground
lobbyManagerService :: ServerWorker ()
lobbyManagerService = do
  flip evalStateT (Map.empty, []) $ forever $ do
    lift . putLog $ "Start lobby manager service"
    users <- withMap $ gets (map fst)
    lift . putLog $ "Lobby manager: users who chose a theme: " ++ show users
    res <- workerRace waitClients waitNewClient
    case res of
      Left (message, client) -> do
        lift . putLog $ "Lobby: new message" ++ show message
        lobbyManagerAction client message
      Right client -> do
        lift . putLog $ "Lobby: new client" ++ show (client^.user)
        topics <- lift $ entityVal <$$> withDB (selectList [] [])
        liftIO $ client^.channels.to writeMsg $ Topics topics

removeClient :: Client -> [(T.Text, Client)] -> [(T.Text, Client)]
removeClient client = filter ((/=(client^.user)) . _user . snd)

-- | Applies an action according to a message type
lobbyManagerAction :: Client -> UserMessage -> LobbyManagerState ()
lobbyManagerAction client Disconnect = do
  withMap $ modify $ removeClient client
  lift . toAuth $ DisconnectMsg Disconnect client
lobbyManagerAction client LogOut = do
  withMap $ modify $ removeClient client
  lift . toAuth $ DisconnectMsg LogOut client
lobbyManagerAction client (SelectTopic topic) = do
  maybeId <- lift . withDB $ entityKey <$$> selectFirst [TopicTitle ==. topic] []
  let status = Status $ if isJust maybeId then Ok else NotFound
  liftIO $ client^.channels.to writeMsg $ status
  case maybeId of
    Just id -> do
      maybeClient <- withMap $ gets (lookup topic)
      case maybeClient of
        Nothing -> do
          withMap $ modify $ ((topic,client):) . removeClient client
          addClient client
        Just client' -> do
          let lobbyInfo = LobbyInfo topic $ [client', client]^..folded.user.to userUsername
          liftIO $ [client',client]^.traversed.channels.to writeMsg $ lobbyInfo
          config <- lift ask
          withMap . modify $ filter ((/=topic) . fst)
          removeClientThread client'
          questions <- lift . withDB $ entityVal <$$> selectList [QuestionTopicId ==. id] []
          liftIO . linkAsync $ runServerWorker (createPlayGround (client', client) questions) config
          return ()
    Nothing -> lift $ toLobby client
lobbyManagerAction client _ = liftIO $ client^.channels.to writeMsg $ Status UnexpectedMessageType
