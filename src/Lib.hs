{-# language OverloadedStrings #-}
module Lib where

import Control.Lens
import Lambda.Var.Indef
import Lambda.Abs.Named
import Lambda.Term
import Data.Unify

term = _Abs # ("x", _Abs # ("y", _Var # "x"))

test :: Either (UnificationError TermF) (Maybe Term)
test = runUnifyT $ do
  var <- fresh
  let term' = UTerm . Right $ _AbsF # ("x", Right $ _AbsF # ("y", Left var))
  unify (unfreeze term) term'
  freeze <$> find term'
