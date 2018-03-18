{-# language LambdaCase #-}
module Lambda.Var (Var(..), nextVar, _Var) where

import Control.Lens

import Lambda.Internal

nextVar :: Var -> Var
nextVar (MkVar name n) = MkVar name (n+1)

_Var :: Prism' Term Var
_Var = prism' Var (\case; Var a -> Just a; _ -> Nothing)
