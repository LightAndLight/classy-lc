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

mkBigStep :: (from -> MaybeK to) -> BigStep from to
mkBigStep = BigStep . step

bigSteps :: [(from -> MaybeK to) -> BigStep from to] -> BigStep from to
bigSteps steps = here
  where
    here = asum $ fmap ($ runBigStep here) steps

runBigStep :: BigStep from to -> from -> MaybeK to
runBigStep = runStep . unBigStep
