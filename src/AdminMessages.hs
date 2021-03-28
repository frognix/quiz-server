{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module AdminMessages where

-- Lens
import Control.Lens

-- Aeson
import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import ExtraTools

data AdminQuestion = AdminQuestion { _text :: Text, _correctAnswer :: Int, _answers :: [Text] }
  deriving (Show, Generic)

makeLenses ''AdminQuestion

instance ToJSON AdminQuestion where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminQuestion where
    parseJSON = genericParseJSON jsonOptions

data AdminTopic = AdminTopic { _title :: Text, _info :: Text, _questions :: [AdminQuestion] }
  deriving (Show, Generic)

makeLenses ''AdminTopic

instance ToJSON AdminTopic where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminTopic where
    parseJSON = genericParseJSON jsonOptions

data AdminMessage = Authorization { _login :: Text, _password :: Text }
                  | EditTopic { _topic :: AdminTopic }
                  | DeleteTopic { _topicTitle :: Text }
                  | GetTopicList
                  deriving (Show, Generic)

makeLenses ''AdminMessage

instance ToJSON AdminMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminMessage where
    parseJSON = genericParseJSON jsonOptions

data AdminServerMessage = TopicList { _topics :: [AdminTopic] }
                        | Status { _status :: Text }
                        deriving (Show, Generic)

makeLenses ''AdminServerMessage

instance ToJSON AdminServerMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminServerMessage where
    parseJSON = genericParseJSON jsonOptions
