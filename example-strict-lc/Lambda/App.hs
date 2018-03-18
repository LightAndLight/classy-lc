{-# language LambdaCase #-}
module Lambda.App where

import Control.Lens
import Lambda.Term

_App :: Prism' Term (Term, Term)
_App = prism' (uncurry App) (\case; App a b -> Just (a, b); _ -> Nothing)
