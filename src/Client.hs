module Client (mkClientChan, writeMsg, readMsg, createClientThread) where

import ServerMessages
import ClientMessages
import Extra.Tools

import Control.Concurrent.Chan
import qualified Network.WebSockets as WS
import Data.Aeson (decode,encode)
import Control.Concurrent.Async (race)
import Channels (ClientChan(..))
import ServerWorker

mkClientChan :: Chan ServerMessage -> Chan UserMessage -> ClientChan
mkClientChan = ClientChan

writeMsg :: ClientChan -> ServerMessage -> IO ()
writeMsg chan = writeChan (inChan chan)

readMsg :: ClientChan -> IO UserMessage
readMsg chan = readChan $ outChan chan

createClientThread :: ClientChan -> WS.Connection -> ServerWorker ()
createClientThread chans conn = websocketThread conn onCreate onDestroy $ do
  result <- liftIO $ race (readChan $ inChan chans) (decode <$> WS.receiveData conn)
  putLog $ "Client: new message: " ++ show result
  liftIO $ case result of
    Left  msg -> WS.sendTextData conn $ encode msg
    Right msg -> case msg of
      Nothing      -> WS.sendTextData conn $ encode $ Status BadMessageStructure
      Just Disconnect -> error "Client disconnected"
      Just message -> writeChan (outChan chans) message
  where onCreate  = putLog "Client connected"
        onDestroy = do
          putLog "Client disconnected"
          liftIO $ writeChan (outChan chans) Disconnect
