module Lambda.App.Semantics.Lazy where

import Control.Applicative
import Control.Lens
import Control.MaybeK
import Lambda.Term
import Lambda.App
import Lambda.Abs
import Lambda.Subst
import Semantics.BigStep

appBigStep :: (Term -> MaybeK Term) -> BigStep Term Term
appBigStep steps =
  mkBigStep $ \from -> do
    (f, x) <- liftMaybe $ from ^? _App
    (name, body) <- liftMaybe . preview _Abs =<< steps f
    steps $ subst [(name, x)] body
