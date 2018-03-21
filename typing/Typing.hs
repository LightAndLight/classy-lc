{-# language GADTs #-}
module Typing where

import Lambda.Term
import Lambda.Type.Context.Indef

data Rule f g m =
  Rule
    (Term f -> Maybe parts)
    (Context g -> parts -> m (Type g))

infer
  :: [Rule f g m]
  -> Context g
  -> Term f
  -> m (Type g)
infer rules ctxt tm = _
