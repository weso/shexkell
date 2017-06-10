module Shexkell.Utils.Either (
    EitherT(..)
  , hoistEither
  , lift
  
  , toEither
  , toEitherT
  , mapLeft
  , mapLeftT
) where


import Control.Monad.Trans.Either hiding (left)
import Control.Monad.Trans (lift)


toEither :: a -> Maybe b -> Either a b
toEither left Nothing  = Left left
toEither _    (Just r) = Right r

toEitherT :: Monad m => a -> Maybe b -> EitherT a m b
toEitherT left = hoistEither . toEither left

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f (Left l) = Left $ f l
mapLeft _ (Right r) = Right r

mapLeftT :: Monad m => (l -> l') -> EitherT l m r -> EitherT l' m r
mapLeftT f = mapEitherT (fmap $ mapLeft f)
