{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Control.Validation where

import Control.Monad.Reader
import Control.Monad.State

import Data.RDF
import Data.List
import Shexkell.Data.ShapeMap (ShapeMap(..), ValidationResult(..))
import Shexkell.Data.ShEx (Schema(..), ShapeExpr(..))

import qualified Data.Map as M

import Debug.Trace


data ValidationContext gr = ValidationContext {
    graph  :: RDF gr
  , schema :: Schema
}

data ValidationState = ValidationState {
    currentNode :: Maybe Node
  , currentMap  :: ShapeMap
}

modifyMap :: (ShapeMap -> ShapeMap) -> ValidationState -> ValidationState
modifyMap f vs = vs { currentMap = f $ currentMap vs }

modifyNode :: (Maybe Node -> Maybe Node) -> ValidationState -> ValidationState
modifyNode f vs = vs { currentNode = f $ currentNode vs }

withNode :: MonadState ValidationState m =>
     Node 
  -> m a
  -> m a
withNode node action = do
  current <- currentNode <$> get
  if Just node == current then
    action
  else do
    modify $ modifyNode $ const $ Just node
    result <- action
    modify $ modifyNode $ const current
    return result

checkNode :: MonadState ValidationState m => Node -> ShapeExpr -> m Bool -> m Bool
checkNode node shape check = withNode node $ do
  shMap <- shapeMap . currentMap <$> get
  case M.lookup node shMap >>= findByShape of
    Just r -> return $ resultValidation r
    Nothing -> check >>= updateValidation node shape
  where 
    findByShape :: [(ShapeExpr, ValidationResult)] -> Maybe ValidationResult
    findByShape = fmap snd . find isShape
    isShape :: (ShapeExpr, ValidationResult) -> Bool
    isShape (shape', _) = shape' == shape

newtype Validation gr a = Validation (ReaderT (ValidationContext gr) (State ValidationState) a)

-- | Runs a validation process
runValidation :: (Rdf gr) =>
     Validation gr a        -- ^ Validation to run
  -> ValidationContext gr   -- ^ Initial context
  -> ShapeMap               -- ^ Input Shape Map
  -> a                      
runValidation (Validation r) ctx = evalState (runReaderT r ctx) . ValidationState Nothing

-- | Runs a validation process and returns the ShapeMap
execValidation :: (Rdf gr) =>
     Validation gr a        -- ^ Validation to run
  -> ValidationContext gr   -- ^ Initial context
  -> ShapeMap               -- ^ Input Shape Map
  -> ShapeMap               -- ^ Output Shape Map
execValidation (Validation r) ctx = currentMap . execState (runReaderT r ctx) . ValidationState Nothing


-- | Update the ShapeMap with the result of a validation
updateValidation :: (
    MonadState ValidationState m) =>
     Node         -- ^ Node that has been validated
  -> ShapeExpr    -- ^ Shape Expression that the node has been validated against
  -> Bool         -- ^ Result of the validation
  -> m Bool       -- ^ Result of the validation with the effect of updating the Shape Map 
updateValidation node shape result = do 
  modify $ modifyMap $ ShapeMap . M.insertWith (++) node [(shape, validationResult result)] . shapeMap
  return result


-- | Translates a Bool validation result to the ValidationResult data type
validationResult :: Bool -> ValidationResult 
validationResult True = Positive 
validationResult False = Negative 

resultValidation :: ValidationResult -> Bool
resultValidation Positive = True
resultValidation Negative = False


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

instance MonadState ValidationState (Validation gr) where
  state run = Validation (lift $ state run)
