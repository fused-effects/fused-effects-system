{-# LANGUAGE GADTs #-}
module Control.Effect.Time
( -- * Time effect
  Time(..)
) where

data Time instant m k where
  Now :: Time instant m instant
