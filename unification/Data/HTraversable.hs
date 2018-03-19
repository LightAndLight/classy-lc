{-# language RankNTypes #-}
module Data.HTraversable where

import Data.HFoldable
import Data.HFunctor

class (HFunctor h, HFoldable h) => HTraversable h where
  htraverse :: Applicative m => (forall x. f x -> m (g x)) -> h f -> m (h g)
