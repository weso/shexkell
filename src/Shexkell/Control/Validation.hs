{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Control.Validation where

import Control.Monad.Reader
import Control.Monad.State

import Data.RDF
import Shexkell.Data.ShapeMap (ShapeMap(..), ValidationResult(..))
import Shexkell.Data.ShEx (Schema(..), ShapeExpr(..))

import qualified Data.Map as M

import Debug.Trace


data ValidationContext gr = ValidationContext {
    graph  :: RDF gr
  , schema :: Schema
}

newtype Validation gr a = Validation (ReaderT (ValidationContext gr) (State ShapeMap) a)

-- | Runs a validation process
runValidation :: (Rdf gr) =>
     Validation gr a        -- ^ Validation to run
  -> ValidationContext gr   -- ^ Initial context
  -> ShapeMap               -- ^ Input Shape Map
  -> a                      
runValidation (Validation r) = evalState . runReaderT r 

-- | Runs a validation process and returns the ShapeMap
execValidation :: (Rdf gr) =>
     Validation gr a        -- ^ Validation to run
  -> ValidationContext gr   -- ^ Initial context
  -> ShapeMap               -- ^ Input Shape Map
  -> ShapeMap               -- ^ Output Shape Map
execValidation (Validation r) = execState . runReaderT r


-- | Update the ShapeMap with the result of a validation
updateValidation :: (MonadState ShapeMap m) => 
     Node         -- ^ Node that has been validated
  -> ShapeExpr    -- ^ Shape Expression that the node has been validated against
  -> Bool         -- ^ Result of the validation
  -> m Bool       -- ^ Result of the validation with the effect of updating the Shape Map 
updateValidation node shape result = do 
  modify $ ShapeMap . M.alter updateNode node . shapeMap
  return result

        -- If the node is not in the Shape Map
  where updateNode Nothing = Just [(shape, validationResult result)]
        -- If the node is in the Shape Map
        updateNode (Just shapeResults) = Just $ updateShape shapeResults 

        hasShape (expectedShape, _) = expectedShape == shape

        -- If the shape is present, combines the results
        combineShape (expectedShape, previousResult)
          | shape == expectedShape = (shape, combine previousResult (validationResult result))
          | otherwise = (expectedShape, previousResult)
        
        -- Updates or inserts the shape with the result in the value for the node
        updateShape shapeResults 
          | any hasShape shapeResults = map combineShape shapeResults
          | otherwise = (shape, validationResult result):shapeResults


-- | Translates a Bool validation result to the ValidationResult data type
validationResult :: Bool -> ValidationResult 
validationResult True = Positive 
validationResult False = Negative 

-- | Combine the input result with the current validation result
combine :: ValidationResult -> ValidationResult -> ValidationResult
combine a b
  | a == b    = Positive
  | otherwise = Negative


----------------------------------------------
-- * Monad instance --------------------------
----------------------------------------------


instance Functor (Validation gr) where
  fmap f a = a >>= return . f

instance Applicative (Validation gr) where
  pure = return
  f <*> a = do
    f' <- f
    a' <- a
    return $ f' a'

instance Monad (Validation gr) where
  return a = Validation (return a)
  (Validation r) >>= f = let g a = case f a of (Validation r') -> r'
    in Validation $ r >>= g

instance MonadReader (ValidationContext gr) (Validation gr) where
  ask = Validation ask
  local f (Validation r) = Validation (local f r)

instance MonadState ShapeMap (Validation gr) where
  state run = Validation (lift $ state run)
