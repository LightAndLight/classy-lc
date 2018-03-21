module Lambda.Abs.Named.Semantics where

import Control.MaybeK
import Lambda.Term
import Lambda.Abs.Named
import Semantics.BigStep

absBigStep :: (TermF f -> MaybeK (TermF f)) -> BigStep (TermF f) (TermF f)
absBigStep _ = mkBigStep (const nothing)
