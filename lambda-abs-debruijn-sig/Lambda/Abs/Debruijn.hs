module Lambda.Abs.Debruijn
  (_AbsF, _Abs, abstract1, Bound, _Bound, bzero)
where

import Lambda.Abs.Debruijn.Indef
import Lambda.Term
import Lambda.Var.Indef

import Control.Lens

_Abs :: Prism' Term Term
_Abs =
  prism'
    ((_AbsF #) . Identity)
    (fmap runIdentity . preview _AbsF)

abstract1 :: Traversable f => Var -> f (TermF f) -> TermF f
abstract1 v =
  (_AbsF #) .
  fmap
    (rewrite
      (\tm -> do
          v' <- tm ^? _Var
          Just $ if v' == v
            then _Bound # bzero
            else _Var # v) .
    transform (over _Bound succ))
