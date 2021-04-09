{-# LANGUAGE OverloadedStrings #-}
module Main where

import Server
import System.Environment
import Text.Read
import Extra.Tools

main :: IO ()
main = do
  args <- readMaybe <$$> getArgs
  config <- mkConfig args
  maybe (return ()) runServer config
  where mkConfig [Just addr] =  Just <$> mkServerConfig addr "./database/quiz-database.sqlite" True
        mkConfig _           = do
          putStrLn "quiz-server address:port"
          putStrLn "Invalid command line arguments"
          putStrLn "Example usage: quiz-server 127.0.0.1:8080"
          return Nothing
