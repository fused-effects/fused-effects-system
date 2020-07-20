module Control.Carrier.Time.System
( -- * Time carrier
  TimeC(..)
) where

newtype TimeC m a = TimeC { runTime :: m a }
