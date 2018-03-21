{-# language OverloadedStrings #-}
module Lib where

import Control.Lens
import Lambda.Var.Indef
import Lambda.Abs.Debruijn
import Lambda.Term
import Data.Unify

term = _Abs # (_Abs # (_Bound # succ bzero))

test :: Either (UnificationError TermF) (Maybe Term)
test = runUnifyT $ do
  var <- fresh
  let term' = UTerm . Right $ _AbsF # (Right $ _AbsF # (Left var))
  unify (unfreeze term) term'
  freeze <$> find term'

test2 :: Term
test2 =
  abstract1 "x" . pure .
  abstract1 "y" . pure $
  _Var # "x"
