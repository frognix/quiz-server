{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ExtraTools where

import Data.Aeson
import GHC.Generics

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
