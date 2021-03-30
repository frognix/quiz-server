{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ExtraTools where

import Data.Aeson
import GHC.Generics ( Generic )
import Control.Applicative ( Alternative, empty )
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import Control.Monad (forever, MonadPlus (mzero))
import Control.Exception (finally, Exception (fromException))
import Control.Concurrent.Async

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe (Just v) _ func = func v
withMaybe _ value  _      = value

withGuard :: (MonadPlus f) => Bool -> f () -> f ()
withGuard True  f  = f >> mzero
withGuard False _  = return ()

jsonOptions :: Options
jsonOptions = defaultOptions
  {
  sumEncoding = TaggedObject "type" "",
  fieldLabelModifier = dropWhile (=='_')
  }

data StatusType = Ok
                | UnexpectedMessageType
                | NotFound
                | AlreadyInDb
                deriving (Show, Generic)

instance ToJSON StatusType where
    toJSON = genericToJSON jsonOptions
instance FromJSON StatusType where
    parseJSON = genericParseJSON jsonOptions

websocketThread :: WS.Connection -> IO () -> IO () -> IO () -> IO ()
websocketThread conn onCreate onDestroy work = do
  onCreate
  flip finally onDestroy $ WS.withPingThread conn 30 (return ()) $ forever work
