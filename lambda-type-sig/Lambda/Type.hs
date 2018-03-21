module Lambda.Type (TypeF(..), Type) where

import Data.Functor.Identity
import Lambda.Type.Indef

type Type = TypeF Identity
