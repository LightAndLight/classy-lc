{-# language RankNTypes #-}
module Data.HFoldable where

import Data.Monoid

class HFoldable h where
  hfoldMap :: Monoid m => (forall x. f x -> m) -> h f -> m

hany :: HFoldable h => (forall x. f x -> Bool) -> h f -> Bool
hany f = getAny . hfoldMap (Any . f)
