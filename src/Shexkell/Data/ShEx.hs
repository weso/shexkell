{-# LANGUAGE RecordWildCards #-}

module Shexkell.Data.ShEx where

import Data.RDF (Node)
import Data.RDF.Namespace (PrefixMapping)

import Shexkell.Data.Common
import Shexkell.Data.ShapeExpr
import Shexkell.Data.TripleExpr

import qualified Data.Map as Map
import Control.DeepSeq (NFData, rnf)

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

instance NFData ShapeExpr where
  rnf NodeConstraint{..} =
    rnf dataType `seq`
    rnf xsFacets `seq`
    rnf values
  rnf Shape{..} =
    rnf virtual `seq`
    rnf closed `seq`
    rnf extra `seq`
    rnf expression `seq`
    rnf inherit `seq`
    rnf semActs
  rnf (ShapeOr lbl exprs) = rnf lbl `seq` rnf exprs
  rnf (ShapeAnd lbl exprs) = rnf lbl `seq` rnf exprs
  rnf (ShapeNot lbl expr) = rnf lbl `seq` rnf expr
  rnf (ShapeRef lbl) = rnf lbl
  rnf _ = ()

instance NFData TripleExpr where
  rnf EachOf{..} =
    rnf expressions `seq`
    rnf cardMin `seq`
    rnf cardMax `seq`
    rnf triplSemActs `seq`
    rnf annotations
  rnf OneOf{..} =
    rnf expressions `seq`
    rnf cardMin `seq`
    rnf cardMax `seq`
    rnf triplSemActs `seq`
    rnf annotations
  rnf TripleConstraint{..} =
    rnf inverse `seq`
    rnf negated `seq`
    rnf predicate `seq`
    rnf valueExpr `seq`
    rnf cardMin `seq`
    rnf cardMax `seq`
    rnf triplSemActs `seq`
    rnf annotations
  rnf (Inclusion lbl) = rnf lbl
