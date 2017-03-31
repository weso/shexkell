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


-- | Parsing in the context of a JSON object
type ObjectParser a = ReaderT Object Parser a


parseObject :: FromJSON a => ObjectParser a -> Object -> Parser a
parseObject = runReaderT

-- | In the context of an object, obtains the value of a field by a
--   specified key
valueOf :: FromJSON a => T.Text -> ObjectParser a
valueOf = getValueWith (.:)

-- | In the context of an object, obtains the value of a field by a
--   specified key. If the field is not present, returns Nothing
valueOpt :: FromJSON a => T.Text -> ObjectParser (Maybe a)
valueOpt = getValueWith (.:?)

getValueWith :: 
     (Object -> T.Text -> Parser a) -- ^ Function that, given an object and a key, returns the value
  -> T.Text                         -- ^ Key
  -> ObjectParser a
getValueWith f key = ask >>= lift . (`f` key)