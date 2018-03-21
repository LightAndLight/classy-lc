module Lambda.App.Debruijn.Semantics.Lazy where

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
    just $
      rewrite
        (\tm -> do
            b <- tm ^? _Bound
            Just $ if b == bzero
              then x
              else _Bound # pred b)
        body
