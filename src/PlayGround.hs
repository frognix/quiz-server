{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module PlayGround where

import Channels
import Database hiding (get)
import Prelude hiding (id)
import ServerMessages hiding (_score,score)
import ClientMessages
import Client

import Control.Concurrent (myThreadId,threadDelay)
import Control.Monad
import Control.Monad.State hiding (state)
import Control.Monad.Reader
import Extra.Tools
import Control.Applicative
import Extra.State hiding (State)
import Data.Text (Text)
import Data.Maybe

import ServerWorker

import Control.Lens

type Field = (PlayGroundState,PlayGroundState,PlayGroundState,PlayGroundState,PlayGroundState,PlayGroundState)

-- cellLens :: CellId -> Lens' Field PlayGroundState
cellLens FirstCell  = _1
cellLens SecondCell = _2
cellLens ThirdCell  = _3
cellLens FourthCell = _4
cellLens FifthCell  = _5
cellLens SixthCell  = _6

nearCells :: CellId -> [CellId]
nearCells FirstCell  = [SecondCell]
nearCells SecondCell = [FirstCell, ThirdCell]
nearCells ThirdCell  = [SecondCell]
nearCells FourthCell = [FifthCell]
nearCells FifthCell  = [FourthCell, SixthCell]
nearCells SixthCell  = [FifthCell]

data PlayGroundState = FirstUser | SecondUser | Start | UserDisconnected UserMessage Client Client | EndGame deriving Eq

instance Show PlayGroundState where
  show FirstUser  = "FirstUser"
  show SecondUser = "SecondUser"
  show Start      = "Start"
  show (UserDisconnected msg _ _) = "UserDisconnected: " ++ show msg
  show EndGame    = "EndGame"

data PlayGround = PlayGround { _field :: Field, _state :: PlayGroundState, _score :: (Int,Int), _questions :: [Question] }

instance Show PlayGround where
  show (PlayGround field state score _) = "PlayGround:\nfield = " ++ show field ++ "\nstate = " ++ show state ++ "\nscore = " ++ show score

makeLenses ''PlayGround

data PlayGroundConfig = PlayGroundConfig { _clients :: (Client, Client) }

makeLenses ''PlayGroundConfig

newtype PlayGroundWorker r = PlayGroundWorker {
  runPGW ::  ReaderT PlayGroundConfig (StateT PlayGround ServerWorker) r
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader PlayGroundConfig, MonadState PlayGround, Alternative, MonadPlus)

runPlayGroundWorker :: PlayGroundWorker a -> PlayGroundConfig -> PlayGround -> ServerWorker a
runPlayGroundWorker action config state = fst <$> runStateT (runReaderT (runPGW action) config) state

liftSW :: ServerWorker a -> PlayGroundWorker a
liftSW action = PlayGroundWorker $ lift . lift $ action

cellNearUser :: CellId -> PlayGroundWorker Bool
cellNearUser cell = do
  curState <- use state
  curField <- use field
  let near = nearCells cell
      near' = elem cell $ case curState of
        FirstUser  -> [FirstCell, SixthCell]
        SecondUser -> [ThirdCell, FourthCell]
  return $ near' || any (\c -> curState==curField^.cellLens c) near

withClient :: Getter Client a -> PlayGroundWorker a
withClient func = do
  curState <- use state
  view $ case curState of
    FirstUser  -> clients._1.func
    SecondUser -> clients._2.func

curClient :: PlayGroundWorker Client
curClient = do
  curState <- use state
  view $ case curState of
    FirstUser  -> clients._1
    SecondUser -> clients._2

anotherClient :: PlayGroundWorker Client
anotherClient = do
  curState <- use state
  view $ case curState of
    FirstUser -> clients._2
    SecondUser -> clients._1

getMessage :: PlayGroundWorker UserMessage
getMessage = do
  chan <- withClient channels
  liftIO $ readMsg chan

sendMessage :: ServerMessage -> PlayGroundWorker ()
sendMessage msg = do
  chan <- withClient channels
  liftIO $ writeMsg chan msg

sendMessageAnother :: ServerMessage -> PlayGroundWorker ()
sendMessageAnother msg = do
  client <- anotherClient
  liftIO $ writeMsg (client^.channels) msg

request :: ServerMessage -> PlayGroundWorker UserMessage
request msg = do
  sendMessage msg
  getMessage

broadcast :: ServerMessage -> PlayGroundWorker ()
broadcast msg = do
  (client, client') <- view clients
  liftIO $ writeMsg (client^.channels) msg
  liftIO $ writeMsg (client'^.channels) msg
  return ()

scoreLens FirstUser = _1
scoreLens SecondUser = _2

allCellsFilled :: PlayGroundWorker Bool
allCellsFilled = do
  playground <- use field
  return $ loop FirstCell playground
  where loop :: CellId -> Field -> Bool
        loop SixthCell f = f^.cellLens SixthCell /= Start
        loop cell      f = f^.cellLens cell /= Start && loop (succ cell) f

plusScore :: Int -> PlayGroundWorker Int
plusScore dx = do
  curState <- use state
  score.scoreLens curState += dx
  use $ score.scoreLens curState

sendGameEndMessage :: PlayGroundWorker ()
sendGameEndMessage = do
  liftSW . putLog $ "Game ended"
  (client1, client2) <- view clients
  (score1, score2) <- use score
  if | score1 > score2 -> do
         liftIO $ writeMsg (client1^.channels) $ GameEnd YouWin  score1
         liftIO $ writeMsg (client2^.channels) $ GameEnd YouLose score2
     | score1 < score2 -> do
         liftIO $ writeMsg (client2^.channels) $ GameEnd YouWin  score2
         liftIO $ writeMsg (client1^.channels) $ GameEnd YouLose score1
     | otherwise       -> broadcast $ GameEnd Draw score1
  liftSW . toLobby $ client1
  liftSW . toLobby $ client2

setCell :: CellId -> PlayGroundWorker ()
setCell cell = do
  curState <- use state
  client <- curClient
  field.cellLens cell .= curState
  newScore <- plusScore 100
  filled <- allCellsFilled
  if filled
    then changeState EndGame
    else broadcast $ LobbyUpdate cell newScore (client^.user.to userUsername)

data LoopState a = End a | EndStatus StatusType a | Next StatusType

getMessageLoop :: (UserMessage -> PlayGroundWorker (LoopState a)) -> PlayGroundWorker a
getMessageLoop func = do
  liftSW . putLog $ "PlayGround: Start message loop"
  msg <- getMessage
  liftSW . putLog $ "PlayGround: Message loop message: " ++ show msg
  case msg of
    Disconnect -> mzero
    LogOut     -> mzero
    _ -> do
      res <- func msg
      case res of
        End v         -> sendMessage (Status Ok) >> return v
        EndStatus s v -> sendMessage (Status s)  >> return v
        Next s        -> sendMessage (Status s)  >> getMessageLoop func

askUser :: PlayGroundWorker ()
askUser = do
  liftSW . putLog $ "PlayGround: Start ask user"
  sendMessage YourMove
  sendMessageAnother NotYourMove
  cell <- getMessageLoop $ \case
    SelectCell cell -> do
      cellNear <- cellNearUser cell
      if cellNear then do
        return $ End cell
      else return $ Next NotFound
    _ -> return $ Next UnexpectedMessageType
  Question qtext _ corAnswer answers <- use $ questions.to head
  questions %= tail
  login <- withClient $ user.to userUsername
  sendMessage $ NewQuestion (UserQuestion qtext answers) login 10
  action <- getMessageLoop $ \case
    SelectAnswer answer -> do
      if answer == corAnswer then do
        return $ End (setCell cell)
      else return $ EndStatus NotFound (return ())
    _ -> return $ Next UnexpectedMessageType
  action
  liftSW . putLog $ "PlayGround: End ask user"
  return ()

tryAction :: PlayGroundWorker () -> PlayGroundWorker ()
tryAction action = mplus action $ do
  client  <- curClient
  client' <- anotherClient
  changeState $ UserDisconnected LogOut client client'

changeState :: PlayGroundState -> PlayGroundWorker ()
changeState newState = do
  curState <- use state
  case curState of
    EndGame             -> return ()
    UserDisconnected {} -> return ()
    _ -> state .= newState

playGroundLoop :: PlayGroundWorker ()
playGroundLoop = do
  liftSW . putLog $ "PlayGround: created"
  let onDestroy = liftSW . putLog $ "Playground: destroyed"
  flip mplus onDestroy $ forever $ do
    curState <- use state
    playGround <- get
    liftSW . putLog $ "PlayGround: new state: " ++ show curState
    liftSW . putLog $ "PlayGround: state: " ++ show playGround
    case curState of
      Start      -> changeState FirstUser
      FirstUser  -> tryAction $ askUser >> changeState SecondUser
      SecondUser -> tryAction $ askUser >> changeState FirstUser
      EndGame    -> sendGameEndMessage >> mzero
      UserDisconnected msg client client' -> do
        liftSW . toAuth $ DisconnectMsg msg client
        liftIO . writeMsg (client'^.channels) $ Status OpponentDisconnected
        liftSW . toLobby $ client'
        mzero

defaultPlayGround = PlayGround {
  _field = (Start,Start,Start,Start,Start,Start),
  _state = Start,
  _score = (0, 0),
  _questions = []
  }

createPlayGround :: (Client, Client) -> [Question] -> ServerWorker ()
createPlayGround clients questions = runPlayGroundWorker playGroundLoop (PlayGroundConfig clients) $ defaultPlayGround { _questions = cycle questions }
