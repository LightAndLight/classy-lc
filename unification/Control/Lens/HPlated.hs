module Control.Lens.HPlated where

import Control.Lens.Traversal

class HPlated h where
  hplate :: Traversal (h f) (h f) (f (h f)) (f (h f))
