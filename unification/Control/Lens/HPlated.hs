{-# language RankNTypes #-}
module Control.Lens.HPlated where

import Control.Applicative
import Control.Lens
import Control.Monad

class HPlated h where
  hplate :: Traversal (h f) (h f) (f (h f)) (f (h f))

htransform :: (HPlated h, Functor f) => (forall x. h x -> h x) -> h f -> h f
htransform = htransformOf hplate

hset
  :: Functor a
  => ASetter (h s) (h t) (a (h s)) (b (h t))
  -> (forall x. a x -> b x) -> h s -> h t
hset l f = go
  where
    go = over l (f . fmap go)

hmapMOf
  :: (Traversable a, Monad m)
  => LensLike (WrappedMonad m) (h s) (h t) (a (h s)) (b (h t))
  -> (forall x. a x -> m (b x))
  -> h s -> m (h t)
hmapMOf l f = go
  where
    go = mapMOf l (f <=< mapM go)

htransformOf
  :: Functor g
  => Traversal (h f) (h f) (g (h g)) (g (h g)) 
  -> (forall x. h x -> h x)
  -> h f -> h f
htransformOf l f = go
  where
    go = f . over (l.mapped) f

htransformOfM
  :: (Traversable g, Monad m)
  => LensLike (WrappedMonad m) (h f) (h f) (g (h g)) (g (h g))
  -> (forall x. h x -> m (h x))
  -> h f -> m (h f)
htransformOfM l f = go
  where
    go = f <=< mapMOf (l.traverse) f
