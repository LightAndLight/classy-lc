{-# language FlexibleContexts, ScopedTypeVariables #-}
module Lambda.Var.Inference where

import Control.Lens ((^?))
import Control.Monad.Except
import Data.Monoid
import Typing
import Lambda.Term
import Lambda.Type
import Lambda.Type.Context.Indef
import Lambda.Var.Indef
import Lambda.Type.CVar

inferVar
  :: forall f g m
   . ( MonadContext (Context g) m
     , MonadError (ContextError g) m
     , LookupConstraints
     )
  => InferenceRule (TermF f) (TypeF g) m
inferVar =
  InferenceRule $ \_ tm -> do
    MkVar a b  <- tm ^? _Var
    Just (CVar a b, inferTy)
  where
    inferTy :: LookupConstraints => CVar -> m (TypeF g)
    inferTy v = lookupCtxt v pure
