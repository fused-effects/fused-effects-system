{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Profile.Tree
( -- * Profile carrier
  runProfile
, reportProfile
, execProfile
, ProfileC(ProfileC)
, Timing(..)
, renderTiming
, mean
, Timings(..)
, renderTimings
, reportTimings
  -- * Profile effect
, module Control.Effect.Profile
) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Writer.Strict
import           Control.Effect.Profile
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HashMap
import           Data.Time.Clock
import           Data.Timing
import           Prelude hiding (sum)

runProfile :: ProfileC m a -> m (Timings, a)
runProfile (ProfileC m) = runWriter m

reportProfile :: Has (Lift IO) sig m => ProfileC m a -> m a
reportProfile m = do
  (t, a) <- runProfile m
  a <$ reportTimings t

execProfile :: Functor m => ProfileC m a -> m Timings
execProfile = fmap fst . runProfile

newtype ProfileC m a = ProfileC { runProfileC :: WriterC Timings m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Profile :+: sig) (ProfileC m) where
  alg = \case
    L (Measure l m k) -> do
      start <- sendM getCurrentTime
      (sub, a) <- ProfileC (censor @Timings (const mempty) (listen (runProfileC m)))
      end <- sendM getCurrentTime
      ProfileC (tell (timing l (end `diffUTCTime` start) sub))
      k a
    R other -> ProfileC (send (handleCoercible other))
    where
    timing ls t = Timings . HashMap.singleton ls . Timing t t t 1
