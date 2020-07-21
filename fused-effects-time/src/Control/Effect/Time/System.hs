{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Time.System
( module Control.Effect.Time
, Instant(..)
, Duration(..)
, since
, time
, sinceEpoch
, era
) where

import Control.Effect.Time
import Data.Fixed
import Data.Time.Clock.System

newtype Instant = Instant { getInstant :: SystemTime }
  deriving (Eq, Ord, Show)

newtype Duration = Duration { getDuration :: Nano }
  deriving (Eq, Fractional, Num, Ord, Real, Show)


since :: Instant -> Instant -> Duration
since (Instant (MkSystemTime bs bns)) (Instant (MkSystemTime as ans)) = Duration (realToFrac (as - bs) + MkFixed (fromIntegral ans - fromIntegral bns))
{-# INLINABLE since #-}

time :: Has (Time Instant) sig m => m a -> m (Duration, a)
time = timeWith since
{-# INLINE time #-}

sinceEpoch :: Has (Time Instant) sig m => m Duration
sinceEpoch = sinceEpochWith since
{-# INLINE sinceEpoch #-}

era :: Has (Time Instant) sig m => m a -> m a
era m = do
  epoch <- now @Instant
  eraFrom epoch m
{-# INLINE era #-}
