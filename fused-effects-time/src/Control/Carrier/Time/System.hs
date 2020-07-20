module Control.Carrier.Time.System
( -- * Time carrier
  TimeC(..)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Effect.Time

newtype TimeC m a = TimeC { runTime :: m a }
