{-# LANGUAGE OverloadedStrings #-}
module UserTests (testUserAPI) where

import ClientConnection
import Test.Hspec

import ClientMessages
import ServerMessages
import Extra.Tools
import Server
import Database

type UserConnection = ClientConnection UserMessage ServerMessage

connectUser :: IO UserConnection
connectUser = connectClient "127.0.0.1" 8080 ""

testUserAPI :: IO ()
testUserAPI = hspec $ describe "User API tests" $ do
  authTest
  selectTopicTest

authTest :: Spec
authTest = describe "User authorization" $ do
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
    msg <- request client $ SelectAnswer 1
    msg `shouldBe` Status UnexpectedMessageType
    closeClient client

selectTopicTest :: Spec
selectTopicTest = describe "Select topic" $ do
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
