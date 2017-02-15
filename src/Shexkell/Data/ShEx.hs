module Shexkell.Data.ShEx
  (
  ) where

import qualified Data.Map as Map


--     prefixes  :: Maybe PrefixMap,
--     base      :: Maybe IRI,
--     startAct  :: Maybe [SemAct],
--     start     :: Maybe ShapeExpr,
--     shapes    :: Maybe (Map.Map ShapeLabel ShapeExpr)
-- } deriving Show
--
-- data ShapeExpr =
--     ShapeOr  [ShapeExpr]
--   | ShapeAnd [ShapeExpr]
--   | ShapeNot ShapeExpr
--   | NodeConstraint
--   {
--     nodeKind :: Maybe [NodeKind],
--     dataType :: Maybe IRI,
--     xsFacets :: [XsFacet],
--     values   :: Maybe [ValueSetValue]
--   }
--   | Shape
--   {
--     virtual    :: Maybe Bool,
--     closed     :: Maybe Bool,
--     extra      :: Maybe [IRI],
--     expression :: Maybe [TripleExpr],
--     inherit    :: Maybe ShapeLabel,
--     semActs    :: Maybe [SemAct]
--   }
--   | ShapeRef ShapeLabel
--   | ShapeExternal
--   deriving Show
