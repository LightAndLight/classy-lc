{-# language KindSignatures #-}
module Lambda.Type.Indef where

import Control.Lens

data TypeF (f :: * -> *)
  = Unit
  deriving Show

instance Traversable f => Plated (TypeF f) where
  plate _ a = pure a
