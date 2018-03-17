{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Control.MaybeK where

import Control.Applicative

newtype MaybeK a = MaybeK { maybeK :: forall r. r -> (a -> r) -> r }
  deriving Functor

liftMaybe :: Maybe a -> MaybeK a
liftMaybe Nothing = nothing
liftMaybe (Just a) = just a

nothing :: MaybeK a
nothing = MaybeK (\r _ -> r)

just :: a -> MaybeK a
just a = MaybeK (\_ f -> f a)

instance Foldable MaybeK where
  foldMap f (MaybeK mf) = mf mempty f

instance Traversable MaybeK where
  traverse f (MaybeK mf) = mf (pure nothing) (fmap just . f)

instance Applicative MaybeK where
  pure = just
  MaybeK mf <*> MaybeK ma =
    mf nothing $ \f ->
    ma nothing $ \a ->
    just (f a)

instance Alternative MaybeK where
  empty = nothing
  MaybeK ma <|> mb = ma mb just

instance Monad MaybeK where
  MaybeK ma >>= f = ma nothing f
