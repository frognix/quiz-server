{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import ClientMessages
import ServerMessages
import Extra.Tools
import Data.Maybe
import Server
import ServerDB

import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets as WS
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Data.Aeson (decode,encode)
import Control.Monad
import Control.Exception
import Data.Typeable
import Control.Concurrent (threadDelay)

data ClientConnection = ClientConnection { inChan :: Chan UserMessage, outChan :: Chan ServerMessage, thread :: Async () }

connectClient :: IO ClientConnection
connectClient = do
  userChan <- newChan
  serverChan <- newChan
  client <- async $ WS.runClient "127.0.0.1" 8080 "" $ \conn -> forever $ do
    res <- race (readChan userChan) (decode <$> WS.receiveData conn)
    case res of
      Left  msg -> WS.sendTextData conn $ encode msg
      Right maybeMsg -> withMaybe maybeMsg (return ()) $ \msg -> writeChan serverChan msg
  return $ ClientConnection userChan serverChan client

closeClient :: ClientConnection -> IO ()
closeClient client = cancel $ thread client

sendMessage :: ClientConnection -> UserMessage -> IO ()
sendMessage client = writeChan (inChan client)

getMessage :: ClientConnection -> IO ServerMessage
getMessage client = readChan (outChan client)

requestWithoutTimer :: ClientConnection -> UserMessage -> IO ServerMessage
requestWithoutTimer client msg = do
  sendMessage client msg
  getMessage client

data TestException = Timeout
  deriving (Typeable, Show)

instance Exception TestException

requestWithTimer :: Int -> ClientConnection -> UserMessage -> IO ServerMessage
requestWithTimer time client msg = do
  res <- race (requestWithoutTimer client msg) (threadDelay $ time * 1000 * 1000)
  case res of
    Left v  -> return v
    Right _ -> expectationFailure "The time for the request is up" >> undefined

request :: ClientConnection -> UserMessage -> IO ServerMessage
request client msg = requestWithTimer 5 client msg

main :: IO ()
main = do
  deleteDB
  initDB
  server <- linkAsync runServer
  fillTables
  authTest
  selectTopicTest
  cancel server


authTest :: IO ()
authTest = hspec $ describe "User Authorization" $ do
  it "should create new user" $ do
    client <- connectClient
    msg <- request client $ Registration "user2" "qwerty"
    msg `shouldBe` Status Ok
    closeClient client
  it "should connect to new user" $ do
    client <- connectClient
    msg <- request client $ Authorization "user2" "qwerty"
    msg `shouldBe` Status Ok
    closeClient client
  it "should not create two users with same username" $ do
    client <- connectClient
    msg <- request client $ Registration "user2" "12345"
    msg `shouldBe` Status AlreadyInDb
    closeClient client
  it "should connect with correct login and password" $ do
    client <- connectClient
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    closeClient client
  it "return error with incorrect login and password" $ do
    client <- connectClient
    msg <- request client $ Authorization "admin" "12345"
    msg `shouldBe` Status NotFound
    closeClient client
  it "should return error on unexpected message" $ do
    client <- connectClient
    msg <- request client $ SelectAnswer 1
    msg `shouldBe` Status UnexpectedMessageType
    closeClient client

selectTopicTest :: IO ()
selectTopicTest = hspec $ describe "Select topic" $ do
  it "should return Ok after selecting an existing topic" $ do
    client <- connectClient
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    topics <- getMessage client
    topics `shouldBe` Topics [Topic "Programming" "Questions about programming"]
    msg <- request client $ SelectTopic "Programming"
    msg `shouldBe` Status Ok
    closeClient client
  it "should return error after selecting a non-existing topic" $ do
    client <- connectClient
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    topics <- getMessage client
    topics `shouldBe` Topics [Topic "Programming" "Questions about programming"]
    msg <- request client $ SelectTopic "Incorrect topic"
    msg `shouldBe` Status NotFound
    closeClient client
