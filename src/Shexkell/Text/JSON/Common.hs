{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Text.JSON.Common where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Control.Monad (unless)

import Shexkell.Data.Common


instance FromJSON ShapeLabel where
  parseJSON = withText "shape label" (return . IRILabel . T.unpack)

instance FromJSON SemAct where
  parseJSON = withObject "SemAct" $ \ o -> do
    assertType "SemAct" o
    name <- o .:  "name"
    code <- o .:? "code"
    return $ SemAct name code

assertType :: String -> Object -> Parser ()
assertType expectedType o = do
  value <- o .: "type" 
  unless (value == expectedType)
    (fail $ "Unexpected type: expected " ++ expectedType) :: Parser ()

