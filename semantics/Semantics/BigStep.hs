{-# language GeneralizedNewtypeDeriving #-}
module Semantics.BigStep where

import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Category (Category)
import Data.Foldable

import Control.MaybeK
import Semantics.Step

newtype BigStep from to = BigStep { unBigStep :: Step from to }
  deriving (Category, Functor, Applicative, Alternative, Monad)

bigStep :: (from -> MaybeK to) -> BigStep from to
bigStep = BigStep . step

bigSteps :: [BigStep from to -> BigStep from to] -> BigStep from to
bigSteps steps = here
  where
    here = asum $ fmap ($ here) steps

runBigStep :: BigStep from to -> from -> MaybeK to
runBigStep = runStep . unBigStep
