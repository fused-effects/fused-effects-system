{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Time
( -- * Time effect
  Time(..)
) where

import Data.Kind (Type)

data Time instant (m :: Type -> Type) k where
  Now :: Time instant m instant
