{-# LANGUAGE OverloadedStrings #-}
module Main where

import Server

main :: IO ()
main = do
  config <- mkServerConfig "./database/quiz-database.sqlite" True
  runServer config
