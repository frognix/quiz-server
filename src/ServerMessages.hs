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
import Database.Schema (Topic)

import Extra.Tools

instance ToJSON Topic where
    toJSON = genericToJSON jsonOptions
instance FromJSON Topic where
    parseJSON = genericParseJSON jsonOptions

data UserQuestion = UserQuestion { _text :: Text, _answers :: [Text] }
  deriving (Show, Generic, Eq)

makeLenses ''UserQuestion

instance ToJSON UserQuestion where
    toJSON = genericToJSON jsonOptions
instance FromJSON UserQuestion where
    parseJSON = genericParseJSON jsonOptions

data GameEndState = YouWin | YouLose | Draw deriving (Show, Generic, Eq)

instance ToJSON GameEndState where
    toJSON = genericToJSON jsonOptions
instance FromJSON GameEndState where
    parseJSON = genericParseJSON jsonOptions


{- Define ServerMessage type -}
data ServerMessage = Status { _status :: StatusType }
                   | Topics { _topics :: [Topic] }
                   | LobbyInfo { _topic :: Text, _players :: [Text] }
                   | YourMove | NotYourMove
                   | LobbyUpdate { _cellId :: CellId, _score :: Int, _player :: Text }
                   | NewQuestion { _question :: UserQuestion, _player :: Text, _time :: Int }
                   | GameEnd { _situation :: GameEndState, _score :: Int }
  deriving (Show, Generic, Eq)

makeLenses ''ServerMessage

instance ToJSON ServerMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON ServerMessage where
    parseJSON = genericParseJSON jsonOptions
