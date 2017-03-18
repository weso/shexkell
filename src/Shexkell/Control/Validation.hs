{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Shexkell.Control.Validation where

import Control.Monad.Reader
import Control.Monad.State

import Data.RDF
import Shexkell.Data.ShEx (Schema(..), ShapeMap)

data ValidationContext gr = ValidationContext {
    graph  :: RDF gr
  , schema :: Schema
}

newtype Validation gr a = Validation (ReaderT (ValidationContext gr) (State ShapeMap) a)

runValidation :: (Rdf gr) =>
     Validation gr a
  -> ValidationContext gr
  -> ShapeMap
  -> a
runValidation (Validation r) ctx = evalState (runReaderT r ctx)

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
