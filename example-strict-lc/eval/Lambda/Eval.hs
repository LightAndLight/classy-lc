module Lambda.Eval where

import Control.Applicative
import Control.MaybeK
import Lambda.Term
import Lambda.Abs.Named.Semantics
import Lambda.App.Named.Semantics.Strict
import Semantics.BigStep

eval :: Term -> Term
eval tm = maybeK semantics tm eval
  where
    semantics =
      runBigStep
        (bigSteps [appBigStep, absBigStep, const empty])
        tm
