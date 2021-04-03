{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import UserTests
import AdminTests

import Extra.Tools
import Server
import ServerWorker
import Database.Actions
import Control.Concurrent.Async (cancel)
import Control.Concurrent

main :: IO ()
main = do
  config <- mkServerConfig "./database/quiz-database.sqlite" False
  flip runServerWorker config $ do
    deleteDB
    initDB
    fillTables
  server <- linkAsync $ runServer config
  threadDelay 500000
  testUserAPI
  testAdminAPI
  cancel server
