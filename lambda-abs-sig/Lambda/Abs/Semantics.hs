module Lambda.Abs.Semantics where

import Control.MaybeK
import Lambda.Term
import Lambda.Abs
import Semantics.BigStep

bigStepAbs :: BigStep Term Term -> BigStep Term Term
bigStepAbs _ = bigStep just
