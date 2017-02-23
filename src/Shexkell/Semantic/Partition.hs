module Shexkell.Semantic.Partition (
    partition
  , partitionM
) where

import Control.Monad (join)
import Control.Monad.Identity

import qualified Data.Set as Set
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Composition

import Prelude hiding (pred)


-- / Find the partition of a set that matches a predicate. Uses partitionM with
--   the Identity monad
partition :: Ord a =>
     (Set.Set a -> Bool)
  -> Set.Set a
  -> Maybe (Set.Set a, Set.Set a)
partition p set = runIdentity $ partitionM (return . p) set

-- | Find the partition of a set that matches a monadic predicate
partitionM :: (Monad m, Ord a) =>
     (Set.Set a -> m Bool)              -- ^ Predicate that the partition must satisfy
  -> Set.Set a                          -- ^ Input set
  -> m (Maybe (Set.Set a, Set.Set a))   -- ^ 2-Tuple with the partition that
                                        --   satisfies the predicate at the left
                                        --   and the remainder at right
partitionM p set = findPartition p (Set.empty, set)

-- | Given an initial partition, try recursively the different combinations until
--   one that satisfies a predicate is found
findPartition :: (Monad m, Ord a) =>
     (Set.Set a -> m Bool)
  -> (Set.Set a, Set.Set a)
  -> m (Maybe (Set.Set a, Set.Set a))
findPartition p current@(left, right) = p left >>= checkPartition where
  checkPartition True  = return $ Just current
  checkPartition False = join . find isJust <$> mapM (findPartition p) (Set.toList $ expand current)


-- / Gets the different combinations from a partition
expand :: Ord a => (Set.Set a, Set.Set a) -> Set.Set (Set.Set a, Set.Set a)
expand (left, right) = Set.map swap right
  where swap element = (element `Set.insert` left, element `Set.delete` right)
