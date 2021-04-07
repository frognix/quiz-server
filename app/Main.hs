{-# LANGUAGE OverloadedStrings #-}
module Main where

import Server

main :: IO ()
main = do
  config <- mkServerConfig (ServerAddress "192.168.3.5" 8080) "./database/quiz-database.sqlite" True
  runServer config
