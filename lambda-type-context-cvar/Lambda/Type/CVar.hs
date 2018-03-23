module Lambda.Type.CVar where

newtype CVar = CVar { unCVar :: String }
  deriving (Eq, Ord, Show)
