{-# language FlexibleContexts #-}
module Lambda.Infer where

import Control.Monad.Except
import Lambda.Type.Indef
import Lambda.Type.Context.Indef
import Lambda.Term.Indef
import Lambda.Var.Inference
import Typing

inferType
  :: ( MonadContext (Context g) m
     , MonadError (ContextError g) m
     , LookupConstraints
     )
  => TermF f -> Maybe (m (TypeF g))
inferType = infer [inferVar]
