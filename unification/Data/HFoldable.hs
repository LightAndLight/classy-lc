{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.HFoldable where

import Data.Monoid
import GHC.Generics

class HFoldable h where
  hfoldMap :: (Foldable f, Monoid m) => (forall x. f x -> m) -> h f -> m

hany :: (HFoldable h, Foldable f) => (forall x. f x -> Bool) -> h f -> Bool
hany f = getAny . hfoldMap (Any . f)

ghfoldMap
  :: ( Generic (l f), GHFoldable (Rep (l f)) f
     , Monoid m, Foldable f
     )
  => (forall x. f x -> m) -> l f -> m
ghfoldMap f = ghfoldMap' f . from

class GHFoldable h f where
  ghfoldMap' :: (Foldable f, Monoid m) => (forall x. f x -> m) -> h a -> m

instance (GHFoldable a f, GHFoldable b f) => GHFoldable (a :+: b) f where
  ghfoldMap' f (L1 a) = ghfoldMap' f a
  ghfoldMap' f (R1 a) = ghfoldMap' f a

instance (GHFoldable a f, GHFoldable b f) => GHFoldable (a :*: b) f where
  ghfoldMap' f (a :*: b) = ghfoldMap' f a <> ghfoldMap' f b

instance GHFoldable U1 f where
  ghfoldMap' f _ = mempty

instance GHFoldable h f => GHFoldable (M1 a b h) f where
  ghfoldMap' f (M1 a) = ghfoldMap' f a

instance {-# OVERLAPPING #-} HFoldable h => GHFoldable (Rec0 (f (h f))) f where
  ghfoldMap' f (K1 a) = f a <> foldMap (hfoldMap f) a

instance {-# OVERLAPPING #-} HFoldable h => GHFoldable (Rec0 (h f)) f where
  ghfoldMap' f (K1 a) = hfoldMap f a

instance {-# OVERLAPPING #-} GHFoldable (Rec0 (f a)) f where
  ghfoldMap' f (K1 a) = f a

instance {-# OVERLAPPABLE #-} GHFoldable (Rec0 a) f where
  ghfoldMap' f _ = mempty
