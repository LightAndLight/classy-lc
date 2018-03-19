{-# language DeriveGeneric #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
module Lambda.Internal where

import Control.Lens
import Data.String
import GHC.Generics

data Var = MkVar String !Int
  deriving (Eq, Ord, Show)

instance IsString Var where
  fromString s = MkVar s 0

data TermF f = Var Var | Abs Var (f (TermF f)) | App (f (TermF f)) (f (TermF f))
  deriving Generic
deriving instance Eq (f (TermF f)) => Eq (TermF f)
deriving instance Ord (f (TermF f)) => Ord (TermF f)
deriving instance Show (f (TermF f)) => Show (TermF f)

instance Traversable f => Plated (TermF f) where
  plate f (Abs v e) = Abs v <$> traverse f e
  plate f (App a b) = App <$> traverse f a <*> traverse f b
  plate f a = pure a
