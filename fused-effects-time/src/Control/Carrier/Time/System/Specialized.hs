module Control.Carrier.Time.System.Specialized
( module Control.Carrier.Time.System
, now
, epoch
) where

import Control.Algebra
import Control.Carrier.Time.System hiding (epoch, now)

now :: Has (Time Instant) sig m => m Instant
now = send Now
{-# INLINE now #-}

epoch :: Has (Time Instant) sig m => m Instant
epoch = send Epoch
{-# INLINE epoch #-}
