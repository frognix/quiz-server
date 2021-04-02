{-# LANGUAGE OverloadedStrings #-}
module AdminTests where

import ClientConnection
import Test.Hspec

import AdminMessages
import Extra.Tools
import Server
import ServerDB

type AdminConnection = ClientConnection AdminMessage AdminServerMessage

connectAdmin :: IO AdminConnection
connectAdmin = connectClient "127.0.0.1" 8080 "/admin"

testAdminAPI :: IO ()
testAdminAPI = hspec $ describe "Admin API tests" $ do
  authTest


authTest :: Spec
authTest = describe "Admin authorization" $ do
  it "should connect admin to server" $ do
    client <- connectAdmin
    msg <- request client $ Authorization "admin" "admin"
    msg `shouldBe` Status Ok
    closeClient client
  it "should reject not admin account" $ do
    client <- connectAdmin
    msg <- request client $ Authorization "user" "1234"
    msg `shouldBe` Status NotFound
    closeClient client
