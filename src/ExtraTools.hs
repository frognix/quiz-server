{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ExtraTools where

import Data.Aeson
import GHC.Generics

maybeIf :: Maybe a -> b -> (a -> b) -> b
maybeIf (Just v) _ func = func v
maybeIf _ value _       = value

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
