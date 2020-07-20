{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Time.System
( -- * Time carrier
  Instant
, Duration(..)
, since
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

type Instant = SystemTime

newtype Duration = Duration { getDuration :: Nano }
  deriving (Eq, Fractional, Num, Ord, Real, Show)


since :: Instant -> Instant -> Duration
since (MkSystemTime bs bns) (MkSystemTime as ans) = Duration (realToFrac (as - bs) + MkFixed (fromIntegral ans - fromIntegral bns))
{-# INLINABLE since #-}


newtype TimeC m a = TimeC { runTime :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TimeC where
  lift = TimeC
  {-# INLINE lift #-}

instance Has (Lift IO) sig m => Algebra (Time SystemTime :+: sig) (TimeC m) where
  alg hdl sig ctx = case sig of
    L Now   -> (<$ ctx) <$> sendIO getSystemTime
    R other -> TimeC (alg (runTime . hdl) other ctx)
  {-# INLINE alg #-}
