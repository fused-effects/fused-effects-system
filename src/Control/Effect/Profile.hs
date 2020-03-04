{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Profile
( -- * Profile effect
  measure
, Profile(..)
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra
import Data.Text

measure :: Has Profile sig m => Text -> m a -> m a
measure l m = send (Measure l m pure)
{-# INLINE measure #-}

data Profile m k
  = forall a . Measure Text (m a) (a -> m k)

deriving instance Functor m => Functor (Profile m)

instance Effect Profile where
  thread ctx hdl (Measure l m k) = Measure l (hdl (m <$ ctx)) (hdl . fmap k)
  {-# INLINE thread #-}
