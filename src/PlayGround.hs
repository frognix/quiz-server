module PlayGround (createPlayGround) where

import Channels
import ServerDB

import Control.Concurrent (myThreadId,threadDelay)
import Control.Monad

createPlayGround :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createPlayGround chans (client1, client2) questions = forever $ do
  threadDelay 1000000
  id <- myThreadId
  putStrLn $ "Game in " ++ show id
