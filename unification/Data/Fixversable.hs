{-# language RankNTypes, KindSignatures #-}
module Data.Fixversable where

class Fixversable h where
  fixverse
    :: Applicative m
    => (f (h f) -> m (g (h g)))
    -> h f
    -> m (h g)
