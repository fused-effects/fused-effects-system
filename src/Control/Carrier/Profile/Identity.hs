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
import Control.Monad (MonadPlus)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype ProfileC m a = ProfileC { runProfile :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans ProfileC where
  lift = ProfileC
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Profile :+: sig) (ProfileC m) where
  alg ctx hdl = \case
    L (Measure _ m k) -> hdl (m <$ ctx) >>= hdl . fmap k
    R other           -> ProfileC (alg ctx (runProfile . hdl) other)
  {-# INLINE alg #-}
