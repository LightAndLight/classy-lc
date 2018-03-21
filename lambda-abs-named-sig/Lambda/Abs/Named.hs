module Lambda.Abs.Named (_AbsF, _Abs) where

import Lambda.Abs.Named.Indef
import Lambda.Term
import Lambda.Var.Indef

import Control.Lens

_Abs :: Prism' Term (Var, Term)
_Abs =
  prism'
    ((_AbsF #) . over _2 Identity)
    (over (mapped._2) runIdentity . preview _AbsF)
