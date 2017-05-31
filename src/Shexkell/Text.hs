module Shexkell.Text where

import Data.String
import Shexkell.Data.ShEx

import Data.Aeson

import Shexkell.Text.JSON.ShexParser ()
import Shexkell.Text.Compact.ShexParser
import Shexkell.Text.Compact.Control
import Shexkell.Text.Compact.PreParser


class ShexParser a where
  parseShex :: String -> a -> Either String Schema


data JSONShexParser = JSONShexParser

data CompactShexParser = CompactShexParser

instance ShexParser JSONShexParser where
  parseShex str _ = eitherDecode $ fromString str

instance ShexParser CompactShexParser where
  parseShex str _ = case p of
    Left err -> Left $ show err
    Right sch -> Right sch
    
    where p = parseShexC shexDoc "Compact shex" =<< parse preparse "Compact shex" str
    