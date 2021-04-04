{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module AdminMessages where

-- Aeson
import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import Extra.Tools

data AdminQuestion = AdminQuestion { text :: Text, correctAnswer :: Int, answers :: [Text] }
  deriving (Show, Generic, Eq)

instance ToJSON AdminQuestion where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminQuestion where
    parseJSON = genericParseJSON jsonOptions

data AdminTopic = AdminTopic { title :: Text, info :: Text, questions :: [AdminQuestion] }
  deriving (Show, Generic, Eq)

instance ToJSON AdminTopic where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminTopic where
    parseJSON = genericParseJSON jsonOptions

data AdminMessage = Authorization { login :: Text, password :: Text }
                  | EditTopic { topic :: AdminTopic }
                  | DeleteTopic { topicTitle :: Text }
                  | GetTopicList
                  deriving (Show, Generic, Eq)

instance ToJSON AdminMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminMessage where
    parseJSON = genericParseJSON jsonOptions

data AdminServerMessage = TopicList { topics :: [AdminTopic] }
                        | Status { status :: StatusType }
                        deriving (Show, Generic, Eq)

instance ToJSON AdminServerMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON AdminServerMessage where
    parseJSON = genericParseJSON jsonOptions
