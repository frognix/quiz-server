module ClientConnection (
  ClientConnection(),
  connectClient,
  closeClient,
  sendMessage,
  getMessage,
  request,
  requestWithoutTimer,
  requestWithTimer
) where

import Extra.Tools
import Test.Hspec

import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets as WS
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Data.Aeson (decode,encode,ToJSON,FromJSON)
import Control.Monad
import Control.Exception
import Data.Typeable
import Control.Concurrent (threadDelay)

data ClientConnection i o = ClientConnection { inChan :: Chan i, outChan :: Chan o, thread :: Async () }

connectClient :: (ToJSON i, FromJSON o) => String -> Int -> String -> IO (ClientConnection i o)
connectClient address port path = do
  userChan <- newChan
  serverChan <- newChan
  client <- async $ WS.runClient address port path $ \conn -> forever $ do
    res <- race (readChan userChan) (decode <$> WS.receiveData conn)
    case res of
      Left  msg -> WS.sendTextData conn $ encode msg
      Right maybeMsg -> withMaybe maybeMsg (return ()) $ \msg -> writeChan serverChan msg
  return $ ClientConnection userChan serverChan client

closeClient :: (ToJSON i, FromJSON o) => ClientConnection i o -> IO ()
closeClient client = cancel $ thread client

sendMessage :: (ToJSON i, FromJSON o) => ClientConnection i o -> i -> IO ()
sendMessage client = writeChan (inChan client)

getMessage :: (ToJSON i, FromJSON o) => ClientConnection i o -> IO o
getMessage client = readChan (outChan client)

request :: (ToJSON i, FromJSON o) => ClientConnection i o -> i -> IO o
request client msg = requestWithTimer 5 client msg

requestWithoutTimer :: (ToJSON i, FromJSON o) => ClientConnection i o -> i -> IO o
requestWithoutTimer client msg = do
  sendMessage client msg
  getMessage client

requestWithTimer :: (ToJSON i, FromJSON o) => Int -> ClientConnection i o -> i -> IO o
requestWithTimer time client msg = do
  res <- race (requestWithoutTimer client msg) (threadDelay $ time * 1000 * 1000)
  case res of
    Left v  -> return v
    Right _ -> expectationFailure "The time for the request is up" >> undefined
