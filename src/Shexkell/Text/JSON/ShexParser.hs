{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shexkell.Text.JSON.ShexParser where

import Data.Foldable (asum)
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad (unless)
import Data.Scientific

import Data.Aeson
import Data.Aeson.Types

import Data.RDF (PrefixMapping(..))

import Shexkell.Data.ShEx
import Shexkell.Data.Common
import Shexkell.Data.TripleExpr

import Shexkell.Text.JSON.Common 
import Shexkell.Text.JSON.NodeConstraint


instance FromJSON Schema where
  parseJSON = withObject "Schema" $ \o -> do
    prefixes <- parsePrefixes <$> o .:? "prefixes"
    base     <- o .:? "iri"
    start    <- o .:?  "start"
    shapes   <- o .:? "shapes"
    let startAct = Nothing
    return Schema{..}


instance FromJSON ShapeExpr where        
  parseJSON = withObject "ShapeExpr" $ \o -> asum [
        parseNodeConstraint o
      , parseShapeOr o
      , parseShapeAnd o
      , parseShapeNot o
      , parseShape o
    ]

instance FromJSON TripleExpr where
  parseJSON = withObject "triple expression" $ \ o -> asum [
        parseEachOf o
      , parseOneOf  o
      , parseTripleConstraint o
    ]

instance FromJSON Max where
  parseJSON (String "unbounded") = return Star
  parseJSON num@(Number _)       = IntMax <$> parseJSON num       
  parseJSON _                    = fail "Expected integer or \"unbounded\""

parsePrefixes :: Maybe (Map.Map T.Text T.Text) -> Maybe [PrefixMapping]
parsePrefixes = fmap (map PrefixMapping . Map.toList)


-------------------------------------------------
-- * Shape Expression
-------------------------------------------------


parseShapeOr :: Object -> Parser ShapeExpr
parseShapeOr o = do
  assertType "ShapeOr" o
  shapeId    <- o .:? "id"
  shapeExprs <- o .: "shapeExprs"
  return $ ShapeOr shapeId shapeExprs
    
parseShapeAnd :: Object -> Parser ShapeExpr
parseShapeAnd o = do
  assertType "ShapeAnd" o
  shapeId    <- o .:? "id"
  shapeExprs <- o .: "shapeExprs"
  return $ ShapeAnd shapeId shapeExprs

parseShapeNot :: Object -> Parser ShapeExpr
parseShapeNot o = do
  assertType "ShapeNot" o
  shapeId <- o .:? "id"
  ShapeNot shapeId <$> o .: "shapeExpr"

parseShape :: Object -> Parser ShapeExpr
parseShape o = do
  assertType "Shape" o
  shapeId    <- o .:? "id"
  closed     <- o .:? "closed"
  extra      <- o .:? "extra"
  expression <- o .:? "expression"
  let virtual = Nothing
  let inherit = Nothing
  let semActs = Nothing

  return Shape{..}


------------------------------------------------------------
-- * Triple Expression
------------------------------------------------------------

parseEachOf :: Object -> Parser TripleExpr
parseEachOf o = do
  assertType "EachOf" o
  expressions <- o .:  "expressions"
  cardMin     <- o .:? "min"
  cardMax     <- o .:? "max"
  return EachOf{..}

parseOneOf :: Object -> Parser TripleExpr
parseOneOf o = do
  assertType "OneOf" o
  expressions <- o .:  "expressions"
  cardMin     <- o .:? "min"
  cardMax     <- o .:? "max"
  return OneOf{..}

parseTripleConstraint :: Object -> Parser TripleExpr
parseTripleConstraint o = do
  assertType "TripleConstraint" o
  inverse   <- o .:? "inverse"
  predicate <- o .:  "predicate"
  valueExpr <- o .:? "valueExpr"
  cardMin   <- o .:? "min"
  cardMax   <- o .:? "max"
  let negated = Nothing
  let triplSemActs = Nothing
  let annotations = Nothing
  return TripleConstraint{..}