module Lambda.Subst where

import Lambda.Term
import Lambda.Var.Indef
import Lambda.Abs

import Control.Lens
import Data.List

subst :: Traversable f => [(Var, TermF f)] -> TermF f -> TermF f
subst subs = go []
  where
    go scope a =
      case a ^? _Var of
        Nothing -> over plate (go $! toListOf (_AbsF._1) a ++ scope) a
        Just v -> maybe a (avoidCapture scope) (lookup v subs)

    avoidCapture scope a =
      case a ^? _Var of
        Nothing -> over plate (avoidCapture $! scope \\ toListOf (_AbsF._1) a) a
        Just v ->
          review _Var .
          head .
          filter (`notElem` scope) $
          iterate nextVar v
