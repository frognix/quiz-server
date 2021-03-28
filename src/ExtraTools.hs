{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ExtraTools where

import Data.Aeson

jsonOptions :: Options
jsonOptions = defaultOptions
  {
  sumEncoding = TaggedObject "type" "",
  fieldLabelModifier = dropWhile (=='_')
  }
