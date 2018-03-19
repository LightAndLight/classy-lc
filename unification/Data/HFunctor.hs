{-# language RankNTypes #-}
module Data.HFunctor where

class HFunctor h where
  hfmap :: (forall x. f x -> g x) -> h f -> h g
