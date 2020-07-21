{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Effect.Time.System
( module Control.Effect.Time
, Instant(..)
, Duration(..)
) where

import Control.Effect.Time
import Data.Fixed
import Data.Time.Clock.System

newtype Instant = Instant { getInstant :: SystemTime }
  deriving (Eq, Ord, Show)

newtype Duration = Duration { getDuration :: Nano }
  deriving (Eq, Fractional, Num, Ord, Real, Show)
