module Control.HMonad where

import Data.HFunctor

class HFunctor h => HMonad h where
  hreturn :: f x -> h f
  hbind :: h f -> (forall x. f x -> h g) -> h g
