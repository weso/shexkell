{-# OPTIONS_HADDOCK ignore-exports #-}

module Shexkell.Semantic.Partition (
    partition
  , partitionM
  , partitionN
  , mapdate
  , square
) where

import Control.Monad.Identity

import qualified Data.Set as Set
import Data.Foldable (asum, toList)
import qualified Data.Sequence as Seq

import Prelude hiding (pred)

import Debug.Trace


-- | Find the partition of a set that matches a predicate. Uses 'partitionM' with
--   the 'Identity' monad
partition :: Ord a =>
     (Set.Set a -> Bool)
  -> Set.Set a
  -> Maybe (Set.Set a, Set.Set a)
partition p = runIdentity . partitionM (return . p)

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
findPartition p current@(left, _) = p left >>= checkPartition where
  checkPartition True  = return $ Just current
  checkPartition False = asum <$> mapM (findPartition p) (Set.toList $ expand current)

-- | Gets the different combinations from a partition
expand :: Ord a => (Set.Set a, Set.Set a) -> Set.Set (Set.Set a, Set.Set a)
expand (left, right) = Set.map swap right
  where swap element = (element `Set.insert` left, element `Set.delete` right)


partitionN :: (Ord a, Monad m) =>
     ([Set.Set a] -> m Bool)
  -> Set.Set a
  -> Int
  -> m (Maybe [Set.Set a])
partitionN pred set size = partitionMany mkPred (Set.toList set) (map (const Set.empty) [1..size])  
  where mkPred sets
          | Set.unions sets /= set = return False
          | otherwise = pred sets

partitionMany :: (Ord a, Monad m) =>
     ([Set.Set a] -> m Bool)     -- ^ Predicate
  -> [a]                         -- ^ Next elements
  -> [Set.Set a]                 -- ^ Current state
  -> m (Maybe [Set.Set a])       -- ^ Partition that satisfies the predicate
partitionMany pred (x:xs) st = pred st >>= checkPartition where
  checkPartition True  = return $ Just st
  checkPartition False = asum <$> mapM (partitionMany pred xs) (mapdate (Set.insert x) (square st ++ [map (const Set.empty) st]))

partitionMany pred [] st = pred st >>= checkLast where
  checkLast True = return $ Just st
  checkLast False = return Nothing



square :: [a] -> [[a]]
square xs = map (const xs) xs

mapdate :: 
     (a -> a)
  -> [[a]]
  -> [[a]]
mapdate f xs = zipWith (\r i -> toList $ Seq.adjust f i (Seq.fromList r)) xs [0..length xs]
