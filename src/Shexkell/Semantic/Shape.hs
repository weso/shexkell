{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Semantic.Shape where

import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.RDF (Triple(..), Node(..), Rdf)
import qualified Data.Set as Set

import Shexkell.Control.Validation
import Control.Monad.State.Class
import Control.Monad.Reader.Class

import Shexkell.Data.ShEx
import Shexkell.Semantic.Neighbourhood




match :: TripleExpr -> ShapeMap -> Triple -> Bool
match OneOf{..} m triple = any (\expr -> match expr m triple) expressions
