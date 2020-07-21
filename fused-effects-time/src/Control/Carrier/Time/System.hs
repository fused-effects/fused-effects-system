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
, runTime
, TimeC(TimeC)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Lift
import Control.Carrier.Reader
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


runTime :: Has (Lift IO) sig m => TimeC m a -> m a
runTime (TimeC m) = do
  epoch <- sendIO getSystemTime
  runReader (Instant epoch) m
{-# INLINE runTime #-}

newtype TimeC m a = TimeC { runTimeC :: ReaderC Instant m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Has (Lift IO) sig m => Algebra (Time Instant :+: sig) (TimeC m) where
  alg hdl sig ctx = case sig of
    L Now     -> (<$ ctx) . Instant <$> sendIO getSystemTime
    L Epoch   -> TimeC (asks (<$ ctx))
    L (Era m) -> do
      epoch <- Instant <$> sendIO getSystemTime
      TimeC (local (const epoch) (runTimeC (hdl (m <$ ctx))))
    R other   -> TimeC (alg (runTimeC . hdl) (R other) ctx)
  {-# INLINE alg #-}
