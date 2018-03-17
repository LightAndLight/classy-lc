{-# language GeneralizedNewtypeDeriving #-}
module Semantics.Step where

import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Category (Category)
import Control.MaybeK

newtype Step from to = Step { unStep :: Kleisli MaybeK from to }
  deriving Category

instance Functor (Step from) where
  fmap f s = step (fmap f . runStep s)

instance Applicative (Step from) where
  pure = step . const . just
  ff <*> fa = step $ \from -> runStep ff from <*> runStep fa from

instance Alternative (Step from) where
  empty = step $ const nothing
  fa <|> fb = step $ \from -> runStep fa from <|> runStep fb from

instance Monad (Step from) where
  ff >>= f = step $ \from -> runStep ff from >>= flip runStep from . f

step :: (from -> MaybeK to) -> Step from to
step = Step . Kleisli

runStep :: Step from to -> from -> MaybeK to
runStep = runKleisli . unStep
