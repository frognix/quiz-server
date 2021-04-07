{-# LANGUAGE OverloadedStrings #-}
module AdminTests where

import ClientConnection
import Test.Hspec

import AdminMessages
import Extra.Tools
import Server
import Database
import Data.Aeson (encode)

type AdminConnection = ClientConnection AdminMessage AdminServerMessage

connectAdmin :: IO AdminConnection
connectAdmin = connectClient "127.0.0.1" 8080 "/admin"

connectRawAdmin :: IO RawConnection
connectRawAdmin = connectRawClient "127.0.0.1" 8080 "/admin"

programmingTopic :: AdminTopic
programmingTopic = AdminTopic "Programming" "Questions about programming" [
  AdminQuestion "What is C++" SecondAnswer ["Programming language",
                                           "The best programming language in the world",
                                           "fish",
                                           "Just three letters"]]

newTopic :: AdminTopic
newTopic = AdminTopic "New topic" "new" []

testAdminAPI :: IO ()
testAdminAPI = hspec $ describe "Admin API tests" $ do
  authTest


authTest :: Spec
authTest = describe "Admin authorization" $ do
  it "shoud reject incorrect message format" $ do
    client <- connectRawAdmin
    msg <- request client "This message is in the wrong format"
    msg `shouldBe` encode (Status BadMessageStructure)
    closeClient client
  it "should reject connection with wrong password" $ do
    client <- connectAdmin
    msg <- request client $ Authorization "admin" "12345"
    msg `shouldBe` Status NotFound
    closeClient client
  it "should reject not admin account" $ do
    client <- connectAdmin
    msg <- request client $ Authorization "user" "1234"
    msg `shouldBe` Status NotFound
    closeClient client
  client <- runIO connectAdmin
  it "should connect admin to server" $ do
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
  it "should get list of topics on server" $ do
    topics <- request client GetTopicList
    topics `shouldBe` TopicList [programmingTopic]
  it "should change topic info" $ do
    msg <- request client $ EditTopic programmingTopic { info = "New info" }
    msg `shouldBe` Status Ok
    topics <- request client GetTopicList
    topics `shouldBe` TopicList [programmingTopic { info = "New info"}]
  it "should add new topic" $ do
    msg <- request client $ EditTopic newTopic
    msg `shouldBe` Status Ok
    topics <- request client GetTopicList
    topics `shouldBe` TopicList [programmingTopic { info = "New info"}, newTopic]
  it "should delete topic" $ do
    msg <- request client $ DeleteTopic $ title newTopic
    msg `shouldBe` Status Ok
    topics <- request client GetTopicList
    topics `shouldBe` TopicList [programmingTopic { info = "New info"}]
  it "should reject deletion of non-existent topic" $ do
    msg <- request client $ DeleteTopic $ title newTopic
    msg `shouldBe` Status NotFound
    closeClient client
