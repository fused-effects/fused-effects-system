module Control.Carrier.Time.System.Specialized
( module Control.Carrier.Time.System
, now
, timeWith
, epoch
) where

import Control.Algebra
import Control.Carrier.Time.System hiding (epoch, now, timeWith)

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
