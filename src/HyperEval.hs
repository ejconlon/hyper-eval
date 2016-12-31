{-# LANGUAGE DeriveGeneric #-}

module HyperEval where

import Data.Text (Text)
import GHC.Generics (Generic)

{-
{
  "version": "0.1.0.0",
  "cells": [
    "html $ toStrict $ renderText $ renderHtml undefined"
  ],
  "importModules": "Analyze\nData.Text.Lazy\nLucid",
  "loadFiles": "",
  "settings": {
    "packageTool": "stack",
    "packagePath": "../stack.yaml",
    "searchPath": ""
  }
}
-}

data Settings = Settings
  { packageTool :: Text
  , packagePath :: Text
  , searchPath :: Text
  } deriving (Show, Eq, Generic)

data Notebook = Notebook
  { version :: Text
  , cells :: [Text]
  , importModules :: Text
  , loadFiles :: Text
  , settings :: Settings
  } deriving (Show, Eq, Generic)

data Options = Options
  { source :: Text
  , dest :: Maybe Text
  } deriving (Show, Eq, Generic)

main :: IO ()
main = putStrLn "hello, world"
