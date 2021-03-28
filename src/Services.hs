{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Services where

import ServerDB
import ServerMessages
import ClientMessages
import Channels
import Control.Lens
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Control.Monad
import Control.Exception (finally)
import Control.Concurrent.Chan

authenticationService :: ServerChans -> IO ()
authenticationService chans = forever $ do
  putStrLn "Start authentication"
  client <- readChan $ chans^.authChan
  print "Client message"
  case client of
    ConnectMsg conn -> do
      putStrLn "--Receive connect message--"
      msg <- readChan $ conn^.outChan
      print msg
      putStrLn "-- End receive connect message--"
    _ -> return ()

lobbyManagerService :: ServerChans -> IO ()
lobbyManagerService chans = return () -- TODO

createLobby :: ServerChans -> (Client, Client) -> [Question] -> IO ()
createLobby chans (client1, client2) questions = undefined