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
import qualified Control.Effect.Time as T

now :: Has (Time Instant) sig m => m Instant
now = T.now
{-# INLINE now #-}

timeWith :: Has (Time Instant) sig m => (Instant -> Instant -> delta) -> m a -> m (delta, a)
timeWith = T.timeWith
{-# INLINE timeWith #-}

epoch :: Has (Time Instant) sig m => m Instant
epoch = T.epoch
{-# INLINE epoch #-}

sinceEpochWith :: Has (Time Instant) sig m => (Instant -> Instant -> delta) -> m delta
sinceEpochWith = T.sinceEpochWith
{-# INLINE sinceEpochWith #-}

eraFrom :: Has (Time Instant) sig m => Instant -> m a -> m a
eraFrom = T.eraFrom
{-# INLINE eraFrom #-}
