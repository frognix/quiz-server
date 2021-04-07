{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Extra.Tools where

import Data.Aeson
import GHC.Generics ( Generic )
import Control.Monad (MonadPlus (mzero))
import Control.Concurrent.Async
import Database.Persist.Sqlite (PersistField, toPersistValue, fromPersistValue, PersistValue(PersistInt64), PersistFieldSql, sqlType, SqlType(SqlInt64))

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
                | AlreadyConnected
                | OpponentDisconnected
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

data CellId = FirstCell | SecondCell | ThirdCell | FourthCell | FifthCell | SixthCell deriving (Eq, Enum, Show, Generic)

instance ToJSON CellId where
    toJSON = genericToJSON jsonOptions
instance FromJSON CellId where
    parseJSON = genericParseJSON jsonOptions

data AnswerId = FirstAnswer | SecondAnswer | ThirdAnswer | FourthAnswer deriving (Eq, Enum, Show, Generic)

instance ToJSON AnswerId where
    toJSON = genericToJSON jsonOptions
instance FromJSON AnswerId where
    parseJSON = genericParseJSON jsonOptions

instance PersistField AnswerId where
  toPersistValue a = PersistInt64 . fromIntegral . fromEnum $ a
  fromPersistValue (PersistInt64 v) | v > 0 || v < 4 = Right . toEnum . fromIntegral $ v
                                    | otherwise      = Left "Expected 0-3 for AnswerId"
  fromPersistValue _ = Left "Expected Int64 for AnswerId"

instance PersistFieldSql AnswerId where
  sqlType _ = SqlInt64
