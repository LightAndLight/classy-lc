module Lambda.Abs.Semantics where

import Control.MaybeK
import Lambda.Term
import Lambda.Abs
import Semantics.BigStep

bigStepAbs :: (Term -> MaybeK Term) -> BigStep Term Term
bigStepAbs _ = mkBigStep (const nothing)
