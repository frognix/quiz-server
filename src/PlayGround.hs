module PlayGround (createPlayGround) where

import Channels
import Database

import Control.Concurrent (myThreadId,threadDelay)
import Control.Monad
import Extra.Tools

import ServerWorker

createPlayGround :: (Client, Client) -> [Question] -> ServerWorker ()
createPlayGround (client1, client2) questions = do
  liftIO . linkAsync $ forever $ do
    threadDelay 1000000
    id <- myThreadId
    putStrLn $ "Game in " ++ show id
  return ()
