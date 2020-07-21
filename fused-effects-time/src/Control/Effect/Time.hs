{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Effect.Time
( -- * Time effect
  now
, timeWith
, epoch
, era
, Time(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

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

epoch :: Has (Time instant) sig m => m instant
epoch = send Epoch
{-# INLINE epoch #-}

era :: forall instant m a sig . Has (Time instant) sig m => m a -> m a
era m = send (Era m :: Time instant m a)
{-# INLINE era #-}

data Time instant m k where
  Now   ::        Time instant m instant
  Epoch ::        Time instant m instant
  Era   :: m a -> Time instant m a
