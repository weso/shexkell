{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Environment
import CommandLine

import Data.Aeson hiding (parseJSON)
import Shexkell hiding (graphFormat)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Either hiding (left)
import Control.Monad.Trans.Class


main :: IO ()
main = do
  validation <- runEitherT validateWithOptions
  case validation of
    Left err  -> putStrLn ("Error: " ++ err)
    Right result -> B.putStrLn $ encode result


-- | Parse the arguments of the program to get the options and run
--   the validation with them
validateWithOptions :: EitherT String IO ShapeMap
validateWithOptions = do
  -- Parse the arguments
  args    <- lift getArgs
  ShexkellOptions{..} <- hoistEither $ fromArgs args

  -- Get the path of the files
  shMap  <- toEitherT "Expected Shape Map path"   shapeMapFile
  graph  <- toEitherT "Expected RDF graph path"   graphFile
  schema <- toEitherT "Expected ShEx schema path" schemaFile

  -- Get the format for the parsing or their default values
  let schFormat  = fromMaybe CompactFormat schemaFormat
  let grphFormat = fromMaybe TurtleFormat  graphFormat

  -- Perform the validation
  lift $ withFile shMap ReadMode $ \ hShapeMap ->
    withFile graph ReadMode $ \ hGraph ->
      withFile schema ReadMode $ \ hSchema ->
        validateIO (ShexOptions schFormat grphFormat) hShapeMap hGraph hSchema


------------------------------------------------------
-- * Options parsing
------------------------------------------------------

data ShexkellOptions = ShexkellOptions {
    shapeMapFile :: Maybe FilePath
  , graphFile    :: Maybe FilePath
  , schemaFile   :: Maybe FilePath

  , schemaFormat :: Maybe ShexFormat
  , graphFormat  :: Maybe GraphFormat
}


instance CLIOptions ShexkellOptions where
  dflt = ShexkellOptions {
      shapeMapFile = Nothing
    , graphFile    = Nothing
    , schemaFile   = Nothing
    , schemaFormat = Nothing
    , graphFormat  = Nothing
  }

  addFlag "compact" opts = opts { schemaFormat = Just CompactFormat } 
  addFlag "json"    opts = opts { schemaFormat = Just JsonFormat }
  addFlag _ opts = opts

  addParam ("rdf", rdf)   opts = opts { graphFile = Just rdf }
  addParam ("shex", shex) opts = opts { schemaFile = Just shex }
  addParam ("map", shMap) opts = opts { shapeMapFile = Just shMap }
  addParam _ opts = opts 


------------------------------------------------------
-- * Utils
------------------------------------------------------

toEither :: a -> Maybe b -> Either a b
toEither left Nothing  = Left left
toEither _    (Just r) = Right r

toEitherT :: Monad m => a -> Maybe b -> EitherT a m b
toEitherT left = hoistEither . toEither left