{-# language LambdaCase #-}
module Lambda.Abs where

import Control.Lens

import Lambda.Term
import Lambda.Var

_Abs :: Prism' Term (Var, Term)
_Abs = prism' (uncurry Abs) (\case; Abs a b -> Just (a, b); _ -> Nothing)
