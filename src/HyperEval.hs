{-# LANGUAGE DeriveGeneric #-}

module HyperEval where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Casing as AC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Language.Haskell.Interpreter as H
import qualified Lucid as L
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
  { source :: FilePath
  , dest :: Maybe FilePath
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

data Output = Output deriving (Show, Eq)

-- XXX TODO
renderOutput :: Output -> L.Html ()
renderOutput _ = return ()

-- XXX TODO
processNotebook :: Notebook -> IO Output
processNotebook nb = return Output

readNotebook :: FilePath -> IO Notebook
readNotebook s = do
  bs <- LBS.readFile s 
  case A.decode bs of
    Nothing -> fail ("bad source: " ++ s)
    Just nb -> return nb

run :: Options -> IO ()
run (Options s d) = do
  nb <- readNotebook s
  o <- processNotebook nb
  let h = renderOutput o
      bs = L.renderBS h
  case d of
    Nothing -> LBC.putStrLn bs
    Just d' -> LBS.writeFile d' bs

exe :: IO ()
exe = O.execParser (O.info options O.fullDesc) >>= run
