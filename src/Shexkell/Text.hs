module Shexkell.Text where

import Data.String
import Shexkell.Data.ShEx

import Data.Aeson

import Shexkell.Text.JSON.ShexParser ()
import Shexkell.Text.Compact.ShexParser


class ShexParser a where
  parseShex :: String -> a -> Either String Schema


data JSONShexParser = JSONShexParser

data CompactShexParser = CompactShexParser

instance ShexParser JSONShexParser where
  parseShex str _ = eitherDecode $ fromString str

instance ShexParser CompactShexParser where
  parseShex str _ = case parse shexDoc "Compact shex" str of
    Left err -> Left $ show err
    Right sch -> Right sch