{-# language DeriveGeneric #-}
module Lambda.Term (TermF(..), Term) where

import Data.Functor.Identity
import Lambda.Internal

type Term = TermF Identity
