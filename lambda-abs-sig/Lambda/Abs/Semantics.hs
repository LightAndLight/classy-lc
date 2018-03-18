module Lambda.Abs.Semantics where

import Control.MaybeK
import Lambda.Term
import Lambda.Abs
import Semantics.BigStep

absBigStep :: (Term -> MaybeK Term) -> BigStep Term Term
absBigStep _ = mkBigStep (const nothing)
