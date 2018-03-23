{-# language LambdaCase #-}
module Lambda.Var.Indef (Var(..), _Var, nextVar, toString) where

import Control.Lens
import Data.Monoid

import Lambda.Internal

nextVar :: Var -> Var
nextVar (MkVar name n) = MkVar name (n+1)

_Var :: Prism' (TermF f) Var
_Var = prism' FVar (\case; FVar a -> Just a; _ -> Nothing)

toString :: Var -> String
toString (MkVar name n) = name <> "_" <> show n
