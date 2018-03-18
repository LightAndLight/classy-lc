{-# language DeriveGeneric #-}
module Lambda.Internal where

import Control.Lens
import Data.String
import GHC.Generics

data Var = MkVar String !Int
  deriving (Eq, Ord, Show)

instance IsString Var where
  fromString s = MkVar s 0

data Term = Var Var | Abs Var Term | App Term Term
  deriving (Eq, Ord, Show, Generic)

instance Plated Term where
  plate = gplate
