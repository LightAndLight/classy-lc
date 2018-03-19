{-# language RankNTypes #-}
module Data.HFoldable where

import Data.Monoid

class HFoldable h where
  hfoldMap :: (Foldable f, Monoid m) => (forall x. f x -> m) -> h f -> m

hany :: (HFoldable h, Foldable f) => (forall x. f x -> Bool) -> h f -> Bool
hany f = getAny . hfoldMap (Any . f)
