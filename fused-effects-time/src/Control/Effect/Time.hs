{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Time
( -- * Time effect
  now
, timeWith
, Time(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)

now :: Has (Time instant) sig m => m instant
now = send Now
{-# INLINE now #-}

timeWith :: Has (Time instant) sig m => (instant -> instant -> delta) -> m a -> m (delta, a)
timeWith with m = do
  start <- now
  a <- m
  end <- now
  let d = with start end
  d `seq` pure (d, a)
{-# INLINE timeWith #-}

data Time instant (m :: Type -> Type) k where
  Now :: Time instant m instant
