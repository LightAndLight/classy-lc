{-# language OverloadedStrings #-}
module Lib where

import Control.Lens
import Lambda.Var.Indef
import Lambda.Abs
import Lambda.Term
import Data.Unify

term = _Abs # ("x", _Var # "x")

test :: Either (UnificationError TermF) (Maybe Term)
test = runUnifyT $ do
  var <- fresh
  let term' = UTerm . Right $ _AbsF # ("x", Left var)
  unify (unfreeze term) term'
  freeze <$> find term'
