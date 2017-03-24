{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Text.JSON.Common where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Control.Monad (unless)

import Shexkell.Data.Common
import Shexkell.Text.JSON.Control


instance FromJSON ShapeLabel where
  parseJSON = withText "shape label" (return . IRILabel . T.unpack)

instance FromJSON SemAct where
  parseJSON = withObject "SemAct" $ parseObject $ do
    assertType "SemAct"
    name <- valueOf  "name"
    code <- valueOpt "code"
    return $ SemAct name code

assertType :: String -> ObjectParser ()
assertType expectedType = do
  value <- valueOf "type" 
  unless (value == expectedType)
    (fail $ "Unexpected type: expected " ++ expectedType) 
