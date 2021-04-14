{-# LANGUAGE OverloadedStrings #-}
module UserTests (testUserAPI) where

import ClientConnection
import Test.Hspec

import ClientMessages
import ServerMessages
import Extra.Tools
import Server
import Database
import Data.Aeson (encode)
import Control.Concurrent
import Data.Text (Text)
import Control.Monad

type UserConnection = ClientConnection UserMessage ServerMessage

connectUser :: IO UserConnection
connectUser = connectClient "127.0.0.1" 8080 ""

connectRawUser :: IO RawConnection
connectRawUser = connectRawClient "127.0.0.1" 8080 ""

testUserAPI :: IO ()
testUserAPI = hspec $ before_ (threadDelay 100000) $ describe "User API tests" $ do
  authTest
  selectTopicTest
  playGroundTest1
  playGroundTest2
  playGroundTest3
  playGroundTest4
  playGroundTest5

authTest :: Spec
authTest = describe "User authorization tests" $ do
  it "shoud reject incorrect message format" $ do
    client <- connectRawUser
    msg <- request client "This message is in the wrong format"
    msg `shouldBe` encode (Status BadMessageStructure)
    closeClient client
  it "should create new user" $ do
    client <- connectUser
    msg <- request client $ Registration "user2" "qwerty"
    msg `shouldBe` Status Ok
    closeClient client
  it "should connect to new user" $ do
    client <- connectUser
    msg <- request client $ Authorization "user2" "qwerty"
    msg `shouldBe` Status Ok
    closeClient client
  it "should not create two users with same username" $ do
    client <- connectUser
    msg <- request client $ Registration "user2" "12345"
    msg `shouldBe` Status AlreadyInDb
    closeClient client
  it "should connect with correct login and password" $ do
    client <- connectUser
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    closeClient client
  it "return error with incorrect login and password" $ do
    client <- connectUser
    msg <- request client $ Authorization "admin" "12345"
    msg `shouldBe` Status NotFound
    closeClient client
  it "should return error on unexpected message" $ do
    client <- connectUser
    msg <- request client $ SelectAnswer FirstAnswer
    msg `shouldBe` Status UnexpectedMessageType
    closeClient client

selectTopicTest :: Spec
selectTopicTest = describe "Select topic tests" $ do
  it "should return Ok after selecting an existing topic" $ do
    client <- connectUser
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    topics <- getMessage client
    topics `shouldBe` Topics [Topic "Programming" "Questions about programming"]
    msg <- request client $ SelectTopic "Programming"
    msg `shouldBe` Status Ok
    closeClient client
  it "should return error after selecting a non-existing topic" $ do
    client <- connectUser
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    topics <- getMessage client
    topics `shouldBe` Topics [Topic "Programming" "Questions about programming"]
    msg <- request client $ SelectTopic "Incorrect topic"
    msg `shouldBe` Status NotFound
    closeClient client

connectTwoUsers :: UserConnection -> UserConnection -> Spec
connectTwoUsers client1 client2 = it "should connect two users" $ do
  msg1 <- request client1 $ Authorization "admin" "admin"
  msg1 `shouldBe` Status Ok
  msg1 <- getMessage client1
  msg1 `shouldBe` Topics [Topic "Programming" "Questions about programming"]
  msg2 <- request client2 $ Authorization "user" "12345"
  msg2 `shouldBe` Status Ok
  msg2 <- getMessage client2
  msg2 `shouldBe` Topics [Topic "Programming" "Questions about programming"]
  msg1 <- request client1 $ SelectTopic "Programming"
  msg1 `shouldBe` Status Ok
  msg2 <- request client2 $ SelectTopic "Programming"
  msg2 `shouldBe` Status Ok

userQuestion :: UserQuestion
userQuestion = UserQuestion "What is C++" ["Programming language",
                                        "The best programming language in the world",
                                        "fish",
                                        "Just three letters"]

answerQuestion :: UserConnection -> CellId -> AnswerId -> Text -> IO ()
answerQuestion client cell answer username = do
    msg <- request client $ SelectCell cell
    msg `shouldBe` Status Ok
    msg <- getMessage client
    msg `shouldBe` NewQuestion userQuestion username 10
    msg <- request client $ SelectAnswer answer
    msg `shouldSatisfy` (\(Status s) -> if answer == SecondAnswer then s == Ok else s == NotFound)

startMove :: UserConnection -> UserConnection -> IO ()
startMove client1 client2 = do
  msg <- getMessage client1
  msg `shouldBe` YourMove
  msg <- getMessage client2
  msg `shouldBe` NotYourMove

lobbyUpdate :: UserConnection -> UserConnection -> CellId -> Text -> IO ()
lobbyUpdate client1 client2 cell username = do
  msg <- getMessage client1
  msg `shouldSatisfy` (\(LobbyUpdate c _ u) -> cell == c && username == u)
  msg <- getMessage client2
  msg `shouldSatisfy` (\(LobbyUpdate c _ u) -> cell == c && username == u)

oneMove :: Bool -> UserConnection -> UserConnection -> CellId -> AnswerId -> Text -> Spec
oneMove isLast client1 client2 cell answer username = do
  it "should start move" $ startMove client1 client2
  it "should answer question" $ answerQuestion client1 cell answer username
  it "should read lobby update" $ when (not isLast && answer == SecondAnswer) $ lobbyUpdate client1 client2 cell username

gameEnd :: UserConnection -> GameEndState -> UserConnection -> GameEndState -> IO ()
gameEnd client1 state1 client2 state2 = do
  msg <- getMessage client1
  msg `shouldSatisfy` (\(GameEnd s _) -> s == state1)
  msg <- getMessage client2
  msg `shouldSatisfy` (\(GameEnd s _) -> s == state2)

playGroundTest1 :: Spec
playGroundTest1 = describe "PlayGround tests 1" $ do
  client1 <- runIO connectUser
  client2 <- runIO connectUser
  connectTwoUsers client1 client2
  it "should return lobby info for users" $ do
    msg1 <- getMessage client1
    msg1 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
    msg2 <- getMessage client2
    msg2 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
  describe "should answer first cell for first user"   $ oneMove False client1 client2 FirstCell  SecondAnswer "admin"
  describe "should answer fourth cell for second user" $ oneMove False client2 client1 FourthCell SecondAnswer "user"
  describe "should answer second cell for first user"  $ oneMove False client1 client2 SecondCell SecondAnswer "admin"
  describe "should answer fifth cell for second user"  $ oneMove False client2 client1 FifthCell  SecondAnswer "user"
  describe "should answer third cell for first user"   $ oneMove False client1 client2 ThirdCell  SecondAnswer "admin"
  describe "should answer sixth cell for second user"  $ oneMove True  client2 client1 SixthCell  SecondAnswer "user"
  it "should end game with draw" $ do
    gameEnd client1 Draw client2 Draw
    closeClient client1
    closeClient client2

playGroundTest2 :: Spec
playGroundTest2 = describe "PlayGround tests 2" $ do
  client1 <- runIO connectUser
  client2 <- runIO connectUser
  connectTwoUsers client1 client2
  it "should return lobby info for users" $ do
    msg1 <- getMessage client1
    msg1 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
    msg2 <- getMessage client2
    msg2 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
  describe "should answer first cell for first user"   $ oneMove False client1 client2 FirstCell  FirstAnswer  "admin"
  describe "should answer fourth cell for second user" $ oneMove False client2 client1 FourthCell SecondAnswer "user"
  describe "should answer second cell for first user"  $ oneMove False client1 client2 FirstCell  SecondAnswer "admin"
  describe "should answer fifth cell for second user"  $ oneMove False client2 client1 FifthCell  SecondAnswer "user"
  describe "should answer third cell for first user"   $ oneMove False client1 client2 SecondCell SecondAnswer "admin"
  describe "should answer sixth cell for second user"  $ oneMove False client2 client1 SixthCell  SecondAnswer "user"
  describe "should answer third cell for first user"   $ oneMove True  client1 client2 ThirdCell  SecondAnswer "admin"
  it "should end game with draw" $ do
    gameEnd client1 Draw client2 Draw
    closeClient client1
    closeClient client2

playGroundTest3 :: Spec
playGroundTest3 = describe "PlayGround tests 3" $ do
  client1 <- runIO connectUser
  client2 <- runIO connectUser
  connectTwoUsers client1 client2
  it "should return lobby info for users" $ do
    msg1 <- getMessage client1
    msg1 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
    msg2 <- getMessage client2
    msg2 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
  describe "should answer first cell for first user"   $ oneMove False client1 client2 FirstCell  FirstAnswer  "admin"
  describe "should answer fourth cell for second user" $ oneMove False client2 client1 FourthCell SecondAnswer "user"
  describe "should answer second cell for first user"  $ oneMove False client1 client2 FirstCell  ThirdAnswer  "admin"
  describe "should answer fifth cell for second user"  $ oneMove False client2 client1 FifthCell  SecondAnswer "user"
  describe "should answer third cell for first user"   $ oneMove False client1 client2 FirstCell  SecondAnswer "admin"
  describe "should answer sixth cell for second user"  $ oneMove False client2 client1 SixthCell  SecondAnswer "user"
  describe "should answer third cell for first user"   $ oneMove False client1 client2 SecondCell SecondAnswer "admin"
  describe "should answer sixth cell for second user"  $ oneMove True  client2 client1 ThirdCell  SecondAnswer "user"
  it "should end game with win of second user" $ do
    gameEnd client1 YouLose client2 YouWin
    closeClient client1
    closeClient client2

playGroundTest4 :: Spec
playGroundTest4 = describe "PlayGround tests 4" $ do
  client1 <- runIO connectUser
  client2 <- runIO connectUser
  connectTwoUsers client1 client2
  it "should return lobby info for users" $ do
    msg1 <- getMessage client1
    msg1 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
    msg2 <- getMessage client2
    msg2 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
  describe "should answer first cell for first user"   $ oneMove False client1 client2 FirstCell  SecondAnswer "admin"
  describe "should answer fourth cell for second user" $ oneMove False client2 client1 FourthCell ThirdAnswer  "user"
  describe "should answer second cell for first user"  $ oneMove False client1 client2 SecondCell SecondAnswer "admin"
  describe "should answer fifth cell for second user"  $ oneMove False client2 client1 FourthCell FirstAnswer  "user"
  describe "should answer third cell for first user"   $ oneMove False client1 client2 ThirdCell  SecondAnswer "admin"
  describe "should answer sixth cell for second user"  $ oneMove False client2 client1 FourthCell SecondAnswer "user"
  describe "should answer third cell for first user"   $ oneMove False client1 client2 SixthCell  SecondAnswer "admin"
  describe "should answer sixth cell for second user"  $ oneMove True  client2 client1 FifthCell  SecondAnswer "user"
  it "should end game with win of first user" $ do
    gameEnd client1 YouWin client2 YouLose
    closeClient client1
    closeClient client2

playGroundTest5 :: Spec
playGroundTest5 = describe "PlayGround tests 5" $ do
  client1 <- runIO connectUser
  client2 <- runIO connectUser
  connectTwoUsers client1 client2
  it "should return lobby info for users" $ do
    msg1 <- getMessage client1
    msg1 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
    msg2 <- getMessage client2
    msg2 `shouldBe` LobbyInfo "Programming" ["admin", "user"]
  describe "should answer first cell for first user"   $ oneMove False client1 client2 FirstCell  SecondAnswer "admin"
  describe "should answer fourth cell for second user" $ oneMove False client2 client1 FourthCell ThirdAnswer  "user"
  describe "should answer second cell for first user"  $ oneMove False client1 client2 SecondCell SecondAnswer "admin"
  it "should send user to lobby after opponent disconnection" $ do
    closeClient client2
    msg <- getMessage client1
    msg `shouldBe` NotYourMove
    msg <- getMessage client1
    msg `shouldBe` Status OpponentDisconnected
    msg <- getMessage client1
    msg `shouldBe` Topics [Topic "Programming" "Questions about programming"]
    closeClient client1
