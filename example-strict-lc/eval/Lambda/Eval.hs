module Lambda.Eval where

import Control.Applicative
import Control.MaybeK
import Lambda.Term
import Lambda.Abs.Semantics
import Lambda.App.Semantics.Strict
import Semantics.BigStep

eval :: Term -> Term
eval tm = maybeK semantics tm eval
  where
    semantics =
      runBigStep
        (bigSteps [appBigStep, absBigStep, const empty])
        tm
