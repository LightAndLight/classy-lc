{-# language LambdaCase #-}
module Lambda.Abs.Debruijn.Indef where

import Control.Lens

import Lambda.Term.Indef

_AbsF :: Prism' (TermF f) (f (TermF f))
_AbsF = prism' Abs (\case; Abs a -> Just a; _ -> Nothing)

type Bound = Int

bzero :: Bound
bzero = 0

_Bound :: Prism' (TermF f) Bound
_Bound = prism' BVar (\case; BVar b -> Just b; _ -> Nothing)
