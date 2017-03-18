{-# LANGUAGE RecordWildCards #-}

module Shexkell.Data.ShEx where

import Data.RDF (Node)
import Data.RDF.Namespace (PrefixMapping)

import Shexkell.Data.Common
import Shexkell.Data.ShapeExpr
import Shexkell.Data.TripleExpr

import qualified Data.Map as Map


data Schema = Schema {
    prefixes  :: Maybe PrefixMapping
  , base      :: Maybe IRI
  , startAct  :: Maybe [SemAct]
  , start     :: Maybe ShapeExpr
  , shapes    :: Maybe [ShapeExpr]
}

data ShapeExpr = NodeConstraint {
    shapeId  :: Maybe ShapeLabel
  , nodeKind :: Maybe [NodeKind]
  , dataType :: Maybe IRI
  , xsFacets :: [XsFacet]
  , values   :: Maybe [ValueSetValue]
} | Shape {
    shapeId    :: Maybe ShapeLabel
  , virtual    :: Maybe Bool
  , closed     :: Maybe Bool
  , extra      :: Maybe [IRI]
  , expression :: Maybe TripleExpr
  , inherit    :: Maybe ShapeLabel
  , semActs    :: Maybe [SemAct]
} | ShapeOr  (Maybe ShapeLabel) [ShapeExpr]
  | ShapeAnd (Maybe ShapeLabel) [ShapeExpr]
  | ShapeNot (Maybe ShapeLabel) ShapeExpr
  | ShapeRef ShapeLabel
  | ShapeExternal (Maybe ShapeLabel)

data TripleExpr = EachOf {
    expressions  :: [TripleExpr]
  , cardMin      :: Maybe Int
  , cardMax      :: Maybe Max
  , triplSemActs :: Maybe [SemAct]
  , annotations  :: Maybe [Annotation]
} | OneOf {
    expressions :: [TripleExpr]
  , cardMin :: Maybe Int
  , cardMax :: Maybe Max
  , triplSemActs :: Maybe [SemAct]
  , annotations :: Maybe [Annotation]
} | TripleConstraint {
    inverse :: Maybe Bool
  , negated :: Maybe Bool
  , predicate :: IRI
  , valueExpr :: Maybe ShapeExpr
  , cardMin :: Maybe Int
  , cardMax :: Maybe Max
  , triplSemActs :: Maybe [SemAct]
  , annotations :: Maybe [Annotation]
} | Inclusion ShapeLabel


newtype ShapeMap = ShapeMap { shapeMap :: Map.Map Node ShapeExpr }


shexId :: ShapeExpr -> Maybe ShapeLabel
shexId NodeConstraint{..} = shapeId
shexId Shape{..} = shapeId
shexId (ShapeOr lbl _) = lbl
shexId (ShapeAnd lbl _) = lbl
shexId (ShapeNot lbl _) = lbl
shexId (ShapeRef _) = Nothing
shexId (ShapeExternal lbl) = lbl


triplMin :: TripleExpr -> Maybe Int
triplMin EachOf{..} = cardMin
triplMin OneOf{..}  = cardMin
triplMin TripleConstraint{..} = cardMin
triplMin _ = Nothing

triplMax :: TripleExpr -> Maybe Max
triplMax EachOf{..} = cardMax
triplMax OneOf{..}  = cardMax
triplMax TripleConstraint{..} = cardMax
triplMax _ = Nothing

withoutCardinality :: TripleExpr -> TripleExpr
withoutCardinality expr@EachOf{} = expr { cardMin = Nothing, cardMax = Nothing }
withoutCardinality expr@OneOf{} = expr { cardMin = Nothing, cardMax = Nothing }
withoutCardinality expr@TripleConstraint{} = expr { cardMin = Nothing, cardMax = Nothing }
withoutCardinality expr = expr
