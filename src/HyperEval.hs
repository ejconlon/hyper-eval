{-# LANGUAGE DeriveGeneric #-}

module HyperEval where

import Control.Exception (throwIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.Casing as AC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hyper as HY
import qualified Language.Haskell.Interpreter as HI
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

data Output = Output
  { modules :: [Text]
  , files :: [Text]
  , results :: [(Text, HY.Graphic)]
  }

interpSetImports :: [Text] -> HI.Interpreter ()
interpSetImports xs = HI.setImports (ys ++ ["Prelude", "Hyper"])
  where ys = T.unpack <$> filter (not . T.null) xs

interpLoadFiles  :: [Text] -> HI.Interpreter ()
interpLoadFiles xs = HI.loadModules ys
  where ys = T.unpack <$> filter (not . T.null) xs

interpEval :: Text -> HI.Interpreter HY.Graphic
interpEval _ = undefined

interpNotebook :: Notebook -> HI.Interpreter Output
interpNotebook = undefined

-- XXX TODO
renderOutput :: Output -> L.Html ()
renderOutput _ = return ()

processNotebook :: Notebook -> IO Output
processNotebook nb = do
  let interp = interpNotebook nb
  result <- HI.runInterpreter interp
  either throwIO pure result

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
