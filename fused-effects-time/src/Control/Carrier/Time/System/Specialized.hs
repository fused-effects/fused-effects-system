module Control.Carrier.Time.System.Specialized
( module Control.Carrier.Time.System
, now
, timeWith
, epoch
, sinceEpochWith
, eraFrom
) where

import Control.Algebra
import Control.Carrier.Time.System hiding (epoch, eraFrom, now, sinceEpochWith, timeWith)

now :: Has (Time Instant) sig m => m Instant
now = send Now
{-# INLINE now #-}

timeWith :: Has (Time Instant) sig m => (Instant -> Instant -> delta) -> m a -> m (delta, a)
timeWith with m = do
  start <- now
  a <- m
  end <- now
  let d = with start end
  d `seq` pure (d, a)
{-# INLINE timeWith #-}

epoch :: Has (Time Instant) sig m => m Instant
epoch = send Epoch
{-# INLINE epoch #-}

sinceEpochWith :: Has (Time Instant) sig m => (Instant -> Instant -> delta) -> m delta
sinceEpochWith with = do
  now <- now
  epoch <- epoch
  let d = with epoch now
  d `seq` pure d
{-# INLINE sinceEpochWith #-}

eraFrom :: Has (Time Instant) sig m => Instant -> m a -> m a
eraFrom t m = send (EraFrom t m)
{-# INLINE eraFrom #-}
