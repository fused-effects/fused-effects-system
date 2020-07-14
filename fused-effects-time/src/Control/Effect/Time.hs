{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Time
( -- * Time effect
  now
, Time(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)

now :: Has (Time instant) sig m => m instant
now = send Now

data Time instant (m :: Type -> Type) k where
  Now :: Time instant m instant
