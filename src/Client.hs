module Client (ClientChan(), mkClientChan, writeMsg, readMsg, createClientThread) where

import ServerMessages
import ClientMessages
import Extra.Tools

import Control.Concurrent.Chan
import qualified Network.WebSockets as WS
import Data.Aeson (decode,encode)
import Control.Concurrent.Async (race)

data ClientChan = ClientChan { inChan :: Chan ServerMessage, outChan :: Chan UserMessage }

mkClientChan :: Chan ServerMessage -> Chan UserMessage -> ClientChan
mkClientChan = ClientChan

writeMsg :: ClientChan -> ServerMessage -> IO ()
writeMsg chan = writeChan (inChan chan)

readMsg :: ClientChan -> IO UserMessage
readMsg chan = readChan $ outChan chan

createClientThread :: ClientChan -> WS.Connection -> IO ()
createClientThread chans conn = websocketThread conn onCreate onDestroy $ do
  putStrLn "Client start read message"
  result <- race (readChan $ inChan chans) (decode <$> WS.receiveData conn)
  putStrLn "Client read message"
  case result of
    Left  msg -> WS.sendTextData conn $ encode msg
    Right msg -> case msg of
      Nothing      -> WS.sendTextData conn $ encode $ Status BadMessageStructure
      Just message -> writeChan (outChan chans) message
  putStrLn "Client answered"
  where onCreate  = putStrLn "Client thread created"
        onDestroy = do
          writeChan (outChan chans) Disconnect
          putStrLn "Client thread destroyed"
