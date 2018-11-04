module Lambda.App.Debruijn.Semantics.Strict where

import Control.Applicative
import Control.Lens
import Control.MaybeK
import Lambda.Term
import Lambda.App
import Lambda.Abs.Debruijn
import Semantics.BigStep

appBigStep :: (Term -> MaybeK Term) -> BigStep Term Term
appBigStep steps =
  mkBigStep $ \from -> do
    (f, x) <- liftMaybe (from ^? _App)
    body <- liftMaybe . preview _Abs =<< (steps f <|> just f)
    x' <- steps x <|> just x
    just $ instantiate1 x' body
