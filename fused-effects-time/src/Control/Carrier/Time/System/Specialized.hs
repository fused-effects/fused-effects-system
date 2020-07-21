module Control.Carrier.Time.System.Specialized
( module Control.Carrier.Time.System
, now
) where

import Control.Algebra
import Control.Carrier.Time.System hiding (epoch, eraFrom, now, timeWith)

now :: Has (Time Instant) sig m => m Instant
now = send Now
{-# INLINE now #-}
