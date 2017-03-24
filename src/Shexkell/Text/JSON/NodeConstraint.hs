{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Shexkell.Text.JSON.NodeConstraint where

import Data.Aeson
import Data.Aeson.Types

import Data.Foldable (asum)
import Control.Applicative
import Data.Scientific
import Data.String
import Data.Maybe (fromMaybe)

import Shexkell.Data.ShEx
import Shexkell.Data.ShapeExpr
import Shexkell.Data.Common

import Shexkell.Text.JSON.Control
import Shexkell.Text.JSON.Common

parseNodeConstraint :: ObjectParser ShapeExpr
parseNodeConstraint = do
  assertType "NodeConstraint"
  shapeId  <- valueOpt  "id"
  nodeKind <- valueOpt "nodeKind"
  dataType <- valueOpt "dataType"
  xsFacets <- fromMaybe [] <$> valueOpt "xsFacet"
  values   <- valueOpt "values"
  return NodeConstraint{..}


instance FromJSON NodeKind where
  parseJSON = withText "Node kind" parseKind where
    parseKind "iri"        = return IRIKind
    parseKind "bnode"      = return BNodeKind
    parseKind "nonliteral" = return NonLiteralKind
    parseKind "literal"    = return LiteralKind
    parseKind _            = fail "Expeced iri, bnode, nonliteral, or literal"

  
instance FromJSON XsFacet where
  parseJSON = withObject "XsFacet" $ parseObject $ asum [
      XsStringFacet  <$> parseStringFacet
    , XsNumericFacet <$> parseNumericFacet
    ]


instance FromJSON NumericLiteral where
  parseJSON = withScientific "Numeric literal" parseNumericLiteral where
    parseNumericLiteral n
      | isInteger n  = NumericInt     <$> parseJSON (Number n)
      | isFloating n = NumericDouble  <$> parseJSON (Number n)
      | otherwise    = NumericDecimal <$> parseJSON (Number n)


instance FromJSON ValueSetValue where
  parseJSON value = 
         ObjectValue <$> (parseJSON value :: Parser ObjectValue)
     <|> withObject "Stem" (parseObject $ do
          assertType "Stem"
          Stem <$> valueOf "stem") value
     <|> withObject "StemRange" (parseObject $ do
           assertType "StemRange"
           stemValue  <- valueOf  "stem"
           exclusions <- valueOpt "exclusions"
           return $ StemRange stemValue exclusions) value


instance FromJSON ObjectValue where     
  parseJSON text@(String _) = IRIValue <$> parseJSON text
  parseJSON _               = fail "Expected text"


instance FromJSON StemValue where  
  parseJSON (Object o)
    | null o    = return Wildcard
    | otherwise = fail "Expected empty object (Wildcard)"

  parseJSON text@(String _) = IRIStem <$> parseJSON text


parseStringFacet :: ObjectParser StringFacet
parseStringFacet = parseLitStringFacet <|> parsePatternFacet

parseLitStringFacet :: ObjectParser StringFacet
parseLitStringFacet =
  asum [
      LitStringFacet "length"    <$> valueOf "length"
    , LitStringFacet "minlength" <$> valueOf "minlength"
    , LitStringFacet "maxlength" <$> valueOf "maxlength"
    ]

parsePatternFacet :: ObjectParser StringFacet
parsePatternFacet = do
  patt    <- valueOf "pattern"
  flags   <- valueOf "flags"
  return $ PatternStringFacet patt flags
    
parseNumericFacet :: ObjectParser NumericFacet
parseNumericFacet = asum [
    parseWithKey MaxExclusive   "maxexclusive"
  , parseWithKey MaxInclusive   "maxinclusive"
  , parseWithKey MinExclusive   "minexclusive"
  , parseWithKey MinInclusive   "mininclusive"
  , parseWithKey TotalDigits    "totaldigits"
  , parseWithKey FractionDigits "fractiondigits"
  ]

parseWithKey :: FromJSON a => (String -> a -> b) -> String -> ObjectParser b
parseWithKey constr key = constr key <$> valueOf (fromString key)