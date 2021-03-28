{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ServerMessages where

-- Lens
import Control.Lens

-- Aeson
import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import ExtraTools

data Topic = Topic { _title :: Text, _info :: Text }
  deriving (Show, Generic)

makeLenses ''Topic

instance ToJSON Topic where
    toJSON = genericToJSON jsonOptions
instance FromJSON Topic where
    parseJSON = genericParseJSON jsonOptions

data Player = Player { _login :: Text, _score :: Int }
  deriving (Show, Generic)

makeLenses ''Player

instance ToJSON Player where
    toJSON = genericToJSON jsonOptions
instance FromJSON Player where
    parseJSON = genericParseJSON jsonOptions

data Cell = Cell { _id :: Int, _playerLogin :: Text }
  deriving (Show, Generic)

makeLenses ''Cell

instance ToJSON Cell where
    toJSON = genericToJSON jsonOptions
instance FromJSON Cell where
    parseJSON = genericParseJSON jsonOptions

data UserQuestion = UserQuestion { _text :: Text, _answers :: [Text] }
  deriving (Show, Generic)

makeLenses ''UserQuestion

instance ToJSON UserQuestion where
    toJSON = genericToJSON jsonOptions
instance FromJSON UserQuestion where
    parseJSON = genericParseJSON jsonOptions

{- Define ServerMessage type -}
data ServerMessage = Status { _status :: Text }
                   | Topics { _topics :: [Topic] }
                   | LobbyInfo { _topic :: Topic, _players :: [Player] }
                   | YourMove
                   | LobbyUpdate { _cells :: [Cell], _players :: [Player] }
                   | NewQuestion { _question :: [UserQuestion], _player :: Text, _time :: Int }
                   | GameEnd { _players :: [Player] }
  deriving (Show, Generic)

makeLenses ''ServerMessage

instance ToJSON ServerMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON ServerMessage where
    parseJSON = genericParseJSON jsonOptions
