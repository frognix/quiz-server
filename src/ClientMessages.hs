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

import ExtraTools

{- Define UserMessage type -}
data UserMessage = Registration { _login :: Text, _password :: Text }
                 | Authorization { _login :: Text, _password :: Text }
                 | SelectTopic { _title :: Text }
                 | SelectCell { _id :: Int }
                 | SelectAnswer { _id :: Int }
                 | Disconnect
                 deriving (Show, Generic)

--Create lenses for UserMessage
makeLenses ''UserMessage

--Implement UserMessage transformation from and to json
instance ToJSON UserMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON UserMessage where
    parseJSON = genericParseJSON jsonOptions
