{-# language ConstraintKinds, FlexibleContexts, KindSignatures #-}
module Lambda.Type.Context.Indef where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import Lambda.Var.Indef
import Lambda.Type.Indef
import Lambda.Type.CVar

import qualified Data.Map as Map

newtype Context f = Context { unContext :: Map CVar (TypeF f) }
type MonadContext = MonadReader

data ContextError (f :: * -> *) = ContextError
  deriving Show

type InsertConstraints = Ord CVar
insertCtxt
  :: (InsertConstraints, MonadContext (Context f) m)
  => CVar
  -> TypeF f
  -> m a -> m a
insertCtxt v ty = local (Context . Map.insert v ty . unContext)

type LookupConstraints = Ord CVar
lookupCtxt
  :: ( LookupConstraints
     , MonadContext (Context f) m
     , MonadError (ContextError f) m
     )
  => CVar
  -> (TypeF f -> m a) -> m a
lookupCtxt v f =
  asks (Map.lookup v . unContext) >>=
  maybe (throwError ContextError) f
