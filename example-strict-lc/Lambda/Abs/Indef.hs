{-# language LambdaCase #-}
module Lambda.Abs.Indef where

import Control.Lens

import Lambda.Term
import Lambda.Var

_AbsF :: Prism' (TermF f) (Var, f (TermF f))
_AbsF = prism' (uncurry Abs) (\case; Abs a b -> Just (a, b); _ -> Nothing)
