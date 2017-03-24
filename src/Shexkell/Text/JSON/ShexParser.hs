{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shexkell.Text.JSON.ShexParser where

import Data.Foldable (asum)
import qualified Data.Map as Map
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types

import Data.RDF (PrefixMapping(..))

import Shexkell.Data.ShEx
import Shexkell.Data.Common
import Shexkell.Data.TripleExpr

import Shexkell.Text.JSON.Common 
import Shexkell.Text.JSON.NodeConstraint
import Shexkell.Text.JSON.Control


instance FromJSON Schema where
  parseJSON = withObject "Schema" $ parseObject $ do
    prefixes <- parsePrefixes <$> valueOpt "prefixes"
    base     <- valueOpt  "iri"
    start    <- valueOpt  "start"
    shapes   <- valueOpt  "shapes"
    startAct <- valueOpt  "startActs"
    return Schema{..}


instance FromJSON ShapeExpr where        
  parseJSON = withObject "ShapeExpr" $ parseObject $ asum [
        parseNodeConstraint
      , parseShapeOr
      , parseShapeAnd
      , parseShapeNot
      , parseShape
    ]

instance FromJSON TripleExpr where
  parseJSON = withObject "triple expression" $ parseObject $ asum [
      parseEachOf
    , parseOneOf
    , parseTripleConstraint
    ]

instance FromJSON Max where
  parseJSON (String "unbounded") = return Star
  parseJSON num@(Number _)       = IntMax <$> parseJSON num       
  parseJSON _                    = fail "Expected integer or \"unbounded\""

instance FromJSON Annotation where
  parseJSON = withObject "Annotation" $ parseObject $ do
    predicate <- valueOf "predicate"
    obj       <- valueOf "object"
    return $ Annotation predicate obj

parsePrefixes :: Maybe (Map.Map T.Text T.Text) -> Maybe [PrefixMapping]
parsePrefixes = fmap (map PrefixMapping . Map.toList)


-------------------------------------------------
-- * Shape Expression
-------------------------------------------------


parseShapeOr :: ObjectParser ShapeExpr
parseShapeOr = do
  assertType "ShapeOr"
  shapeId    <- valueOpt "id"
  shapeExprs <- valueOf  "shapeExprs"
  return $ ShapeOr shapeId shapeExprs
    
parseShapeAnd :: ObjectParser ShapeExpr
parseShapeAnd = do
  assertType "ShapeAnd"
  shapeId    <- valueOpt "id"
  shapeExprs <- valueOf  "shapeExprs"
  return $ ShapeAnd shapeId shapeExprs

parseShapeNot :: ObjectParser ShapeExpr
parseShapeNot = do
  assertType "ShapeNot"
  shapeId <- valueOpt "id"
  ShapeNot shapeId <$> valueOf "shapeExpr"

parseShape :: ObjectParser ShapeExpr
parseShape = do
  assertType "Shape"
  shapeId    <- valueOpt "id"
  closed     <- valueOpt "closed"
  extra      <- valueOpt "extra"
  expression <- valueOpt "expression"
  semActs    <- valueOpt "semActs"
  let virtual = Nothing
  let inherit = Nothing

  return Shape{..}


------------------------------------------------------------
-- * Triple Expression
------------------------------------------------------------


parseTripleConstraint :: ObjectParser TripleExpr
parseTripleConstraint = do
  assertType "TripleConstraint"
  inverse      <- valueOpt "inverse"
  predicate    <- valueOf  "predicate"
  valueExpr    <- valueOpt "valueExpr"
  cardMin      <- valueOpt "min"
  cardMax      <- valueOpt "max"
  annotations  <- valueOpt "annotations"
  triplSemActs <- valueOpt "semActs"
  let negated = Nothing
  return TripleConstraint{..}

parseEachOf :: ObjectParser TripleExpr
parseEachOf = do
  assertType "EachOf" 
  expressions  <- valueOf  "expressions"
  cardMin      <- valueOpt "min"
  cardMax      <- valueOpt "max"
  annotations  <- valueOpt "annotations"
  triplSemActs <- valueOpt "semActs"
  return EachOf{..}

parseOneOf :: ObjectParser TripleExpr 
parseOneOf = do
  assertType "OneOf"
  expressions  <- valueOf  "expressions"
  cardMin      <- valueOpt "min"
  cardMax      <- valueOpt "max"
  triplSemActs <- valueOpt "semActs"
  annotations  <- valueOpt "annotations"
  return OneOf{..}