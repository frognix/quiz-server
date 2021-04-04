{-# LANGUAGE MultiParamTypeClasses #-}
module ClientConnection (
  ClientConnection(),
  RawConnection,
  connectClient,
  connectRawClient,
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
import Data.ByteString.Lazy.Internal
import Data.Text

data ClientConnection i o = ClientConnection { inChan :: Chan i, outChan :: Chan o, thread :: Async () }

type RawConnection = ClientConnection ByteString ByteString

connectClient' :: (i -> ByteString) -> (ByteString -> Maybe o) -> String -> Int -> String -> IO (ClientConnection i o)
connectClient' toText fromText address port path = do
  userChan <- newChan
  serverChan <- newChan
  client <- async $ WS.runClient address port path $ \conn -> forever $ do
    res <- race (readChan userChan) (fromText <$> WS.receiveData conn)
    case res of
      Left  msg -> WS.sendTextData conn $ toText msg
      Right maybeMsg -> withMaybe maybeMsg (return ()) $ \msg -> writeChan serverChan msg
  return $ ClientConnection userChan serverChan client

connectClient :: (ToJSON i, FromJSON o) => String -> Int -> String -> IO (ClientConnection i o)
connectClient = connectClient' encode decode

connectRawClient :: String -> Int -> String -> IO RawConnection
connectRawClient = connectClient' id Just

closeClient :: ClientConnection i o -> IO ()
closeClient client = cancel $ thread client

sendMessage :: ClientConnection i o -> i -> IO ()
sendMessage client = writeChan (inChan client)

getMessage :: ClientConnection i o -> IO o
getMessage client = readChan (outChan client)

request :: ClientConnection i o -> i -> IO o
request = requestWithTimer 5

requestWithoutTimer :: ClientConnection i o -> i -> IO o
requestWithoutTimer client msg = do
  sendMessage client msg
  getMessage client

requestWithTimer :: Int -> ClientConnection i o -> i -> IO o
requestWithTimer time client msg = do
  res <- race (requestWithoutTimer client msg) (threadDelay $ time * 1000 * 1000)
  case res of
    Left v  -> return v
    Right _ -> expectationFailure "The time for the request is up" >> undefined
