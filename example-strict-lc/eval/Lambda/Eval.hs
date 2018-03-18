module Lambda.Eval where

import Control.MaybeK
import Lambda.Term
import Lambda.Abs.Semantics
import Lambda.App.Semantics.Lazy
import Semantics.BigStep

eval :: Term -> Term
eval tm = maybeK (runBigStep (bigSteps [appBigStep, absBigStep]) tm) tm eval
