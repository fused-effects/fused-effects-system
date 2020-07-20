{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Profile.Flat
( -- * Profile carrier
  runProfile
, reportProfile
, execProfile
, ProfileC(ProfileC)
, Label
, Timing(..)
, renderTiming
, mean
, Timings(..)
, renderTimings
, reportTimings
  -- * Profile effect
, module Control.Effect.Profile
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Lift
import Control.Carrier.Time.System
import Control.Carrier.Writer.Church
import Control.Effect.Profile
import Control.Monad (MonadPlus)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Timing
import Prelude hiding (lookup)

runProfile :: Applicative m => ProfileC m a -> m (Timings, a)
runProfile = runWriter (curry pure) . runProfileC
{-# INLINE runProfile #-}

reportProfile :: Has (Lift IO) sig m => ProfileC m a -> m a
reportProfile m = do
  (t, a) <- runProfile m
  a <$ reportTimings t
{-# INLINE reportProfile #-}

execProfile :: Applicative m => ProfileC m a -> m Timings
execProfile = execWriter . runProfileC
{-# INLINE execProfile #-}

newtype ProfileC m a = ProfileC { runProfileC :: WriterC Timings m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance Has (Time Instant) sig m => Algebra (Profile :+: sig) (ProfileC m) where
  alg hdl sig ctx = case sig of
    L (Measure l m) -> do
      (sub, (duration, a)) <- ProfileC (listen @Timings (runProfileC (time (hdl (m <$ ctx)))))
      let t = lookup l sub
      -- subtract re-entrant measurements so we don’t count them twice
      a <$ ProfileC (tell (timing l (maybe duration ((duration -) . getTotal . total) t)))
    R other         -> ProfileC (alg (runProfileC . hdl) (R other) ctx)
    where
    timing l t = singleton l (Timing (unit t) (unit t) (unit t) t mempty)
  {-# INLINE alg #-}
