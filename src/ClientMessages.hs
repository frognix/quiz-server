{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ClientMessages where

-- Lens
import Control.Lens

-- Aeson
import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import Extra.Tools

{- Define UserMessage type -}
data UserMessage = Registration { _login :: Text, _password :: Text }
                 | Authorization { _login :: Text, _password :: Text }
                 | SelectTopic { _title :: Text }
                 | SelectCell { _cellId :: CellId }
                 | SelectAnswer { _answerId :: AnswerId }
                 | QuitGame
                 | LogOut
                 | Disconnect
                 deriving (Show, Generic, Eq)

--Create lenses for UserMessage
makeLenses ''UserMessage

--Implement UserMessage transformation from and to json
instance ToJSON UserMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON UserMessage where
    parseJSON = genericParseJSON jsonOptions
