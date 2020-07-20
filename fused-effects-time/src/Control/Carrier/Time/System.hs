{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Time.System
( -- * Time carrier
  TimeC(..)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Applicative (Alternative)
import Control.Effect.Time
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))

newtype TimeC m a = TimeC { runTime :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans TimeC where
  lift = TimeC
