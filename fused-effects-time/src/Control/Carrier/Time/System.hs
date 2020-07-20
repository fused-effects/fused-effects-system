{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Time.System
( -- * Time carrier
  Instant(..)
, Duration(..)
, since
, time
, TimeC(..)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Lift
import Control.Effect.Time
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
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


newtype TimeC m a = TimeC { runTime :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TimeC where
  lift = TimeC
  {-# INLINE lift #-}

instance Has (Lift IO) sig m => Algebra (Time Instant :+: sig) (TimeC m) where
  alg hdl sig ctx = case sig of
    L Now   -> (<$ ctx) . Instant <$> sendIO getSystemTime
    R other -> TimeC (alg (runTime . hdl) other ctx)
  {-# INLINE alg #-}
