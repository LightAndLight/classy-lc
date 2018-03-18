module Lambda.Subst where

import Lambda.Term
import Lambda.Var
import Lambda.Abs

import Control.Lens
import Data.List

subst :: [(Var, Term)] -> Term -> Term
subst subs = go []
  where
    go scope a =
      case a ^? _Var of
        Nothing -> over plate (go $! toListOf (_Abs._1) a ++ scope) a
        Just v -> maybe a (avoidCapture scope) (lookup v subs)

    avoidCapture scope a =
      case a ^? _Var of
        Nothing -> over plate (avoidCapture $! scope \\ toListOf (_Abs._1) a) a
        Just v ->
          review _Var .
          head .
          filter (`notElem` scope) $
          iterate nextVar v
