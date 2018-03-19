{-# language DeriveGeneric #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
module Lambda.Internal where

import Control.Lens
import Data.HFunctor
import Data.HFoldable
import Data.HTraversable
import Data.Fixversable
import Data.Monoid
import Data.String
import Data.Unify
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

instance Unifiable TermF where
  equations (App a b) (App c d) = Just [(a, c), (b, d)]
  equations (Abs v a) (Abs v' b)
    | v == v' = Just [(a, b)]
    | otherwise = Nothing
  equations (Var (MkVar a n)) (Var (MkVar b n'))
    | a == b && n == n' = Just []
    | otherwise = Nothing
  equations _ _ = Nothing

instance HFunctor TermF where
  hfmap f (App a b) = App (hfmap f <$> f a) (hfmap f <$> f b)
  hfmap f (Abs a b) = Abs a (hfmap f <$> f b)
  hfmap f (Var v) = Var v

instance HFoldable TermF where
  hfoldMap f (App a b) =
    f a <> foldMap (hfoldMap f) a <> f b <> foldMap (hfoldMap f) b
  hfoldMap f (Abs _ b) = f b <> foldMap (hfoldMap f) b
  hfoldMap _ (Var _) = mempty

instance HTraversable TermF where
  htraverse f (App a b) = App <$> (f a >>= traverse (htraverse f)) <*> (f b >>= traverse (htraverse f))
  htraverse f (Abs a b) = Abs a <$> (f b >>= traverse (htraverse f))
  htraverse _ (Var v) = pure $ Var v

instance Fixversable TermF where
  fixverse f (App a b) = App <$> f a <*> f b
  fixverse f (Abs a b) = Abs a <$> f b
  fixverse f (Var v) = pure $ Var v
