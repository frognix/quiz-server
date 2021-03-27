{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module APIModels where

-- Lens
import Control.Lens

-- Aeson
import Data.Aeson
import GHC.Generics
import Data.Text (Text)

-- Other
import qualified Data.Char as Char

jsonOptions :: Options
jsonOptions = defaultOptions
  {
  sumEncoding = TaggedObject "type" "",
  fieldLabelModifier = dropWhile (=='_')
  }

{- Define UserMessage type -}
data UserMessage = Registration { _login :: Text, _password :: Text }
                 | Authorization { _login :: Text, _password :: Text }
                 deriving (Show, Generic)

--Create lenses for UserMessage
makeLenses ''UserMessage

--Implement UserMessage transformation from and to json
instance ToJSON UserMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON UserMessage where
    parseJSON = genericParseJSON jsonOptions

{- Define ServerMessage type -}
data ServerMessage = Status { _text :: Text }
  deriving (Show, Generic)

makeLenses ''ServerMessage

instance ToJSON ServerMessage where
    toJSON = genericToJSON jsonOptions
instance FromJSON ServerMessage where
    parseJSON = genericParseJSON jsonOptions
