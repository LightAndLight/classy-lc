module Lambda.Term (TermF(..), Term) where

import Data.Functor.Identity
import Lambda.Term.Indef

type Term = TermF Identity
