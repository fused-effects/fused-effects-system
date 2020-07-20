{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Time.System
( -- * Time carrier
  TimeC(..)
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
import Data.Time.Clock.System

newtype TimeC m a = TimeC { runTime :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TimeC where
  lift = TimeC
  {-# INLINE lift #-}

instance Has (Lift IO) sig m => Algebra (Time SystemTime :+: sig) (TimeC m) where
  alg hdl sig ctx = case sig of
    L Now            -> (<$ ctx) <$> sendIO getSystemTime
    L (TimeWith f m) -> do
      start <- sendIO getSystemTime
      a <- hdl (m <$ ctx)
      end <- sendIO getSystemTime
      let d = f start end
      d `seq` pure ((,) d <$> a)
    R other          -> TimeC (alg (runTime . hdl) other ctx)
  {-# INLINE alg #-}
