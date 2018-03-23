{-# language ExistentialQuantification #-}
module Typing where

import Data.Foldable (asum)

data InferenceRule tm ty m
  = forall parts
  . InferenceRule
  { unInferenceRule
    :: [InferenceRule tm ty m]
    -> tm
    -> Maybe (parts, parts -> m ty)
  }

infer :: [InferenceRule tm ty m] -> tm -> Maybe (m ty)
infer rules tm =
  asum $
  fmap (\(InferenceRule r) -> fmap (snd <*> fst) $ r rules tm) rules
