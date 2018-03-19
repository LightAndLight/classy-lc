{-# language LambdaCase #-}
module Lambda.App.Indef where

import Control.Lens
import Lambda.Term.Indef

_AppF :: Prism' (TermF f) (f (TermF f), f (TermF f))
_AppF = prism' (uncurry App) (\case; App a b -> Just (a, b); _ -> Nothing)
