{-# language StandaloneDeriving, UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Data.Unify
  ( UVar
  , FreshCount
  , UTerm(..)
  , fresh
  , freeze
  , unfreeze
  , find
  , occurs
  , unify
  , UnifyT
  , runUnifyT
  , Unifiable(..)
  , UnificationError(..)
  , AsUnificationError(..)
  )
where

import Control.Lens
import Control.Lens.HPlated
import Control.HMonad
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Coerce
import Data.Either
import Data.Equivalence.Monad
import Data.Foldable (traverse_)
import Data.Functor.Classes
import Data.HFoldable
import Data.HFunctor
import Data.Functor.Identity
import Data.HTraversable
import Data.Monoid

newtype UVar = UVar { unUVar :: Int }
  deriving (Eq, Show, Ord)

newtype UTerm f = UTerm { unUTerm :: Either UVar (f (Either UVar)) }
deriving instance Eq (f (Either UVar)) => Eq (UTerm f)
deriving instance Ord (f (Either UVar)) => Ord (UTerm f)
deriving instance Show (f (Either UVar)) => Show (UTerm f)

data UnificationError f
  = CannotUnify (f (Either UVar)) (f (Either UVar))
  | OccursError UVar (f (Either UVar))
deriving instance Eq (f (Either UVar)) => Eq (UnificationError f)
deriving instance Ord (f (Either UVar)) => Ord (UnificationError f)
deriving instance Show (f (Either UVar)) => Show (UnificationError f)
makeClassyPrisms ''UnificationError

newtype FreshCount = FreshCount { unFreshCount :: Int }
  deriving (Eq, Show, Ord)

fresh :: UnifyT s e f UVar
fresh = UnifyT . lift $ do
  n <- unFreshCount <$> get
  modify $! (FreshCount . (+1) . unFreshCount)
  pure $ UVar n

-- instance (forall g. Traversable g => Plated (f g)) => Plated (UTerm f) where
instance HPlated f => Plated (UTerm f) where
  plate f (UTerm t) =
    UTerm <$>
    traverse
      (fmap (^?! _Right) . go (fmap unUTerm . f . UTerm) . Right)
      t
    where
      go f = either (f . Left) (fmap Right . hplate f)

unfreeze :: HFunctor f => f Identity -> UTerm f
unfreeze = UTerm . Right . hfmap (Right . runIdentity)

freeze :: HTraversable f => UTerm f -> Maybe (f Identity)
freeze = (htraverse go . runIdentity) <=< (go . unUTerm)
  where
    go = either (const Nothing) (Just . Identity)

-- (forall g. Traversable g => Plated (f g)), ... => 
find :: (HPlated f, Ord (f (Either UVar))) => UTerm f -> UnifyT s e f (UTerm f)
find = traverseOf plate find <=< (UnifyT . classDesc)

occurs :: HFoldable f => UVar -> UTerm f -> Bool
occurs var (UTerm tm) =
  eqFun tm (Left var) ||
  any (hany (eqFun $ Left var)) tm
  where
    eqFun = liftEq (\_ _ -> True)

class Unifiable f where
  equations :: f g -> f h -> Maybe [(g (f g), h (f h))]

-- forall g. Traversable g => Plated (f g))
unify
  :: ( HPlated f
     , Ord (f (Either UVar))
     , HFoldable f
     , Unifiable f
     , AsUnificationError e f
     )
  => UTerm f -> UTerm f -> UnifyT s e f ()
unify tm1 tm2 = do
  tm1' <- unUTerm <$> find tm1
  tm2' <- unUTerm <$> find tm2
  unifyEach (tm1', tm2')
  where
    unifyEach (Left _, Left _) = pure ()
    unifyEach (Left var, Right tm)
      | occurs var (UTerm $ Right tm) = UnifyT . throwError $ _OccursError # (var, tm)
      | otherwise = UnifyT $ equate (UTerm $ Left var) (UTerm $ Right tm)
    unifyEach (Right tm, Left var)
      | occurs var (UTerm $ Right tm) = UnifyT . throwError $ _OccursError # (var, tm)
      | otherwise = UnifyT $ equate (UTerm $ Right tm) (UTerm $ Left var)
    unifyEach (Right tmA, Right tmB) =
      maybe
        (UnifyT . throwError $ _CannotUnify # (tmA, tmB))
        (traverse_ unifyEach)
        (equations tmA tmB)

newtype UnifyT s e f a
  = UnifyT
  { unUnifyT :: EquivT s (UTerm f) (UTerm f) (StateT FreshCount (Except e)) a
  }
  deriving (Functor, Applicative, Monad)

runUnifyT :: (AsUnificationError e f, MonadError e m, HFoldable f) => (forall s. UnifyT s e f a) -> m a
runUnifyT m =
  either throwError pure $
  runExcept (evalStateT (runEquivT id combine (unUnifyT m)) (FreshCount 0))
  where
    countLefts = getSum . hfoldMap (either (const $ Sum 1) mempty)

    combine (UTerm tm1) (UTerm tm2) =
      UTerm $ case (tm1, tm2) of
        (Left tm1', Left _) -> tm1
        (Left _, Right tm2') -> tm2
        (Right tm1', Left _) -> tm1
        (Right tm1', Right tm2') ->
          case compare (countLefts tm1') (countLefts tm2') of
            GT -> tm2
            _ -> tm1
