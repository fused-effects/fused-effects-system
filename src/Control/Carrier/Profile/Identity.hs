{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Profile.Identity
( -- * Profiling carrier
  runProfile
, ProfileC(ProfileC)
  -- * Profile effect
, module Control.Effect.Profile
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Profile
import Control.Monad.Fix
import Control.Monad.IO.Class

newtype ProfileC m a = ProfileC { runProfile :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Algebra sig m => Algebra (Profile :+: sig) (ProfileC m) where
  alg = \case
    L (Measure _ m k) -> m >>= k
    R other           -> ProfileC (send (handleCoercible other))
