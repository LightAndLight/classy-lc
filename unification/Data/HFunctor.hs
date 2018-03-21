{-# language RankNTypes #-}
module Data.HFunctor where

class HFunctor h where
  hfmap :: (Functor f, Functor g) => (forall x. f x -> g x) -> h f -> h g
