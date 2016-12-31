{-# LANGUAGE DeriveGeneric #-}

module HyperEval where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Casing as AC
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Options.Applicative as O

data Settings = Settings
  { packageTool :: Text
  , packagePath :: Text
  , searchPath :: Text
  } deriving (Show, Eq, Generic)

camelCase :: AT.Options
camelCase = AC.aesonDrop 0 AC.camelCase

instance A.ToJSON Settings where
   toJSON = A.genericToJSON camelCase

instance A.FromJSON Settings where
   parseJSON = A.genericParseJSON camelCase

data Notebook = Notebook
  { version :: Text
  , cells :: [Text]
  , importModules :: Text
  , loadFiles :: Text
  , settings :: Settings
  } deriving (Show, Eq, Generic)

instance A.ToJSON Notebook where
   toJSON = A.genericToJSON camelCase

instance A.FromJSON Notebook where
   parseJSON = A.genericParseJSON camelCase

data Options = Options
  { source :: String
  , dest :: Maybe String
  } deriving (Show, Eq)

options :: O.Parser Options
options = Options
     <$> O.strOption
         ( O.long "source"
        <> O.metavar "SOURCE"
        <> O.help "path to source notebook" )
     <*> O.optional
         ( O.strOption
           ( O.long "dest"
          <> O.help "path to destination html"
          <> O.metavar "DEST" ) )

run :: Options -> IO ()
run _ = putStrLn "hello, world"

main :: IO ()
main = O.execParser (O.info options O.fullDesc) >>= run
