module Shexkell.Text.JSON.Control (
    ObjectParser
  , parseObject
  , valueOf
  , valueOpt
) where

import Control.Monad.Reader 

import Data.Aeson
import Data.Aeson.Types

import qualified Data.Text as T


type ObjectParser a = ReaderT Object Parser a


parseObject :: FromJSON a => ObjectParser a -> Object -> Parser a
parseObject = runReaderT

valueOf :: FromJSON a => T.Text -> ObjectParser a
valueOf = getValueWith (.:)

valueOpt :: FromJSON a => T.Text -> ObjectParser (Maybe a)
valueOpt = getValueWith (.:?)

getValueWith :: (Object -> T.Text -> Parser a) -> T.Text -> ObjectParser a
getValueWith f key = ask >>= \ o -> lift $ f o key