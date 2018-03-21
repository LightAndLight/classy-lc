{-# language DeriveGeneric #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
module Lambda.Internal where

import Control.Lens
import Control.Lens.HPlated
import Data.HFunctor
import Data.HFoldable
import Data.HTraversable
import Data.Monoid
import Data.String
import Data.Unify
import GHC.Generics

data Var = MkVar String !Int
  deriving (Eq, Ord, Show)

instance IsString Var where
  fromString s = MkVar s 0

data TermF f
  = FVar Var
  | BVar Int
  | Abs (f (TermF f))
  | App (f (TermF f)) (f (TermF f))
  deriving Generic
deriving instance Eq (f (TermF f)) => Eq (TermF f)
deriving instance Ord (f (TermF f)) => Ord (TermF f)
deriving instance Show (f (TermF f)) => Show (TermF f)

instance Traversable f => Plated (TermF f) where
  plate f (Abs e) = Abs <$> traverse f e
  plate f (App a b) = App <$> traverse f a <*> traverse f b
  plate f a = pure a

instance Unifiable TermF where
  equations (App a b) (App c d) = Just [(a, c), (b, d)]
  equations (Abs a) (Abs b) = Just [(a, b)]
  equations (FVar (MkVar a n)) (FVar (MkVar b n'))
    | a == b && n == n' = Just []
    | otherwise = Nothing
  equations (BVar a) (BVar b)
    | a == b = Just []
    | otherwise = Nothing
  equations _ _ = Nothing

instance HFunctor TermF where
  hfmap f (App a b) = App (hfmap f <$> f a) (hfmap f <$> f b)
  hfmap f (Abs b) = Abs (hfmap f <$> f b)
  hfmap f (FVar v) = FVar v
  hfmap f (BVar v) = BVar v

-- instance HFoldable TermF where
  -- hfoldMap f (App a b) =
    -- f a <> foldMap (hfoldMap f) a <> f b <> foldMap (hfoldMap f) b
  -- hfoldMap f (Abs _ b) = f b <> foldMap (hfoldMap f) b
  -- hfoldMap _ (Var _) = mempty

instance HFoldable TermF where
  hfoldMap = ghfoldMap

instance HTraversable TermF where
  htraverse f (App a b) = App <$> (f a >>= traverse (htraverse f)) <*> (f b >>= traverse (htraverse f))
  htraverse f (Abs b) = Abs <$> (f b >>= traverse (htraverse f))
  htraverse _ (FVar v) = pure $ FVar v
  htraverse _ (BVar v) = pure $ BVar v

instance HPlated TermF where
  hplate f (App a b) = App <$> f a <*> f b
  hplate f (Abs b) = Abs <$> f b
  hplate f (FVar v) = pure $ FVar v
  hplate f (BVar v) = pure $ BVar v
