module Lambda.App where

import Lambda.App.Indef
import Lambda.Term

import Control.Lens

_App :: Prism' Term (Term, Term)
_App =
  prism'
    ((_AppF #) . over both Identity)
    (over (mapped.both) runIdentity . preview _AppF)
