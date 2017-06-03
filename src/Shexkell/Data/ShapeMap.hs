{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Shexkell.Data.ShapeMap where


import qualified Data.Map as Map
import           Data.RDF
import           Data.Maybe (fromJust)

import           Shexkell.Data.ShEx hiding (shapes)
import           Shexkell.Data.Common


-- | Relation of nodes and shapes that the nodes are expected to match or have
--   been validated
newtype ShapeMap = ShapeMap { shapeMap :: Map.Map Node [(ShapeExpr, ValidationResult)] }
  deriving (Show)

-- | Result of the validation
data ValidationResult = Positive | Negative
  deriving (Show, Eq, Ord)


-- | Instances of this class can be used to build a ShapeMap when given a
--   graph and schema
class ShapeMapRef a n s | a -> n, a -> s where

  -- | Create a ShapeMap
  mkMap :: Rdf gr => RDF gr -> Schema -> a -> ShapeMap
  mkMap _ schema ref = ShapeMap $ Map.fromList $ map findShapes $ nodes ref  where
    findShapes ns = (mkNode ref ns, map mkShape (shapes ref ns))
    mkShape sh = (findShape sh, result ref sh)
    findShape sh = fromJust $ findShapeByLabel (mkShapeLabel ref sh) schema

  -- | Get the structure that lists the nodes of the shape map
  nodes :: a -> [n]
  -- | Get the shape references to validate agains a given node
  shapes :: a -> n -> [s]

  -- | Create a node from a node reference
  mkNode :: a -> n -> Node
  -- | Create a Shape Label from a shape reference
  mkShapeLabel :: a -> s -> ShapeLabel
  -- | Create a expected validation result from a shape reference
  result :: a -> s -> ValidationResult