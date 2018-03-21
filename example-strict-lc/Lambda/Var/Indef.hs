{-# language LambdaCase #-}
module Lambda.Var.Indef (Var(..), nextVar, _Var) where

import Control.Lens

import Lambda.Internal

nextVar :: Var -> Var
nextVar (MkVar name n) = MkVar name (n+1)

_Var :: Prism' (TermF f) Var
_Var = prism' FVar (\case; FVar a -> Just a; _ -> Nothing)
