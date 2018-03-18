module Lambda.Abs.Semantics where

import Control.MaybeK
import Lambda.Term
import Lambda.Abs
import Semantics.BigStep

absBigStep :: (TermF f -> MaybeK (TermF f)) -> BigStep (TermF f) (TermF f)
absBigStep _ = mkBigStep (const nothing)
