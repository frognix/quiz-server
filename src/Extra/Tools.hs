{-# LANGUAGE DeriveGeneric #-}
module Extra.Tools where

import Data.Aeson
import GHC.Generics ( Generic )
import Control.Monad (MonadPlus (mzero))
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
                | BadMessageStructure
                deriving (Show, Generic, Eq)

instance ToJSON StatusType where
    toJSON = genericToJSON jsonOptions
instance FromJSON StatusType where
    parseJSON = genericParseJSON jsonOptions

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap2

linkAsync :: IO a -> IO (Async a)
linkAsync action = do
  thread <- async action
  link thread
  return thread
