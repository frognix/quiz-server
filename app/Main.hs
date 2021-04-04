{-# LANGUAGE OverloadedStrings #-}
module Main where

import Server

main :: IO ()
main = do
  config <- mkServerConfig (ServerAddress "127.0.0.1" 8080) "./database/quiz-database.sqlite" True
  runServer config
