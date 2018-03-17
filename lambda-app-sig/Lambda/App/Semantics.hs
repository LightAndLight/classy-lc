module Lambda.App.Semantics where

import Control.Applicative
import Control.Lens
import Control.MaybeK
import Lambda.Term
import Lambda.App
import Lambda.Abs
import Lambda.Subst
import Semantics.BigStep

beta :: Term -> Term -> Maybe Term
beta f x = do
  (name, body) <- f ^? _Abs
  pure $ subst [(name, x)] body

appBigStep :: BigStep Term Term -> BigStep Term Term
appBigStep steps =
  bigStep $ \from -> do
    (f, x) <- liftMaybe $ from ^? _App
    f' <- runBigStep steps f <|> just f
    x' <- runBigStep steps x <|> just x
    liftMaybe $ beta f' x'
