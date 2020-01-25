{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Carrier.Writer.Strict
import Control.Effect.Profile
import Control.Monad (MonadPlus)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Time.Clock
import Data.Timing
import Prelude hiding (lookup, sum)

runProfile :: ProfileC m a -> m (Timings, a)
runProfile (ProfileC m) = runWriter m

reportProfile :: Has (Lift IO) sig m => ProfileC m a -> m a
reportProfile m = do
  (t, a) <- runProfile m
  a <$ reportTimings t

execProfile :: Functor m => ProfileC m a -> m Timings
execProfile = fmap fst . runProfile

newtype ProfileC m a = ProfileC { runProfileC :: WriterC Timings m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Profile :+: sig) (ProfileC m) where
  alg = \case
    L (Measure l m k) -> do
      start <- sendM getCurrentTime
      (sub, a) <- ProfileC (listen @Timings (runProfileC m))
      end <- sendM getCurrentTime
      let t = lookup l sub
      -- subtract re-entrant measurements so we don’t count them twice
      ProfileC (tell (timing l ((end `diffUTCTime` start) - maybe 0 sum t)))
      k a
    R other -> ProfileC (send (handleCoercible other))
    where
    timing l t = singleton l (Timing t t t 1 mempty)
  {-# INLINE alg #-}
