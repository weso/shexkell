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

import Shexkell.Text.JSON.Common

parseNodeConstraint :: Object -> Parser ShapeExpr
parseNodeConstraint o = do
  assertType "NodeConstraint" o
  shapeId  <- o .:?  "id"
  nodeKind <- o .:? "nodeKind"
  dataType <- o .:? "dataType"
  xsFacets <- fromMaybe [] <$> (o .:? "xsFacet")
  values   <- o .:? "values"
  return NodeConstraint{..}


instance FromJSON NodeKind where
  parseJSON = withText "Node kind" parseKind where
    parseKind "iri"        = return IRIKind
    parseKind "bnode"      = return BNodeKind
    parseKind "nonliteral" = return NonLiteralKind
    parseKind "literal"    = return LiteralKind
    parseKind _            = fail "Expeced iri, bnode, nonliteral, or literal"

  
instance FromJSON XsFacet where
  parseJSON = withObject "XsFacet" $ \ o -> 
    (XsStringFacet  <$> parseStringFacet  o) <|> 
    (XsNumericFacet <$> parseNumericFacet o)


instance FromJSON NumericLiteral where
  parseJSON = withScientific "Numeric literal" parseNumericLiteral where
    parseNumericLiteral n
      | isInteger n  = NumericInt     <$> parseJSON (Number n)
      | isFloating n = NumericDouble  <$> parseJSON (Number n)
      | otherwise    = NumericDecimal <$> parseJSON (Number n)


instance FromJSON ValueSetValue where
  parseJSON value = 
         ObjectValue <$> (parseJSON value :: Parser ObjectValue)
     <|> withObject "Stem" (\ o -> do
          assertType "Stem" o
          Stem <$> o .: "stem") value
     <|> withObject "StemRange" (\ o -> do
           assertType "StemRange" o
           stemValue  <- o .:  "stem"
           exclusions <- o .:? "exclusions"
           return $ StemRange stemValue exclusions) value


instance FromJSON ObjectValue where     
  parseJSON text@(String _) = IRIValue <$> parseJSON text
  parseJSON _               = fail "Expected text"


instance FromJSON StemValue where  
  parseJSON (Object o)
    | null o    = return Wildcard
    | otherwise = fail "Expected empty object (Wildcard)"

  parseJSON text@(String _) = IRIStem <$> parseJSON text


parseStringFacet :: Object -> Parser StringFacet
parseStringFacet o = parseLitStringFacet o <|> parsePatternFacet o

parseLitStringFacet :: Object -> Parser StringFacet
parseLitStringFacet o =
  asum [
      LitStringFacet "length"    <$> o .: "length"
    , LitStringFacet "minlength" <$> o .: "minlength"
    , LitStringFacet "maxlength" <$> o .: "maxlength"
    ]

parsePatternFacet :: Object -> Parser StringFacet
parsePatternFacet o = do
  patt    <- o .: "pattern"
  flags   <- o .: "flags"
  return $ PatternStringFacet patt flags
    
parseNumericFacet :: Object -> Parser NumericFacet
parseNumericFacet o = asum [
    parseWithKey MaxExclusive   "maxexclusive"   o
  , parseWithKey MaxInclusive   "maxinclusive"   o
  , parseWithKey MinExclusive   "minexclusive"   o
  , parseWithKey MinInclusive   "mininclusive"   o
  , parseWithKey TotalDigits    "totaldigits"    o
  , parseWithKey FractionDigits "fractiondigits" o 
  ]

parseWithKey :: FromJSON a => (String -> a -> b) -> String -> Object -> Parser b
parseWithKey constr key obj = constr key <$> obj .: fromString key