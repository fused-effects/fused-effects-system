{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.Timing
( Timing(..)
, mean
, variance
, stdDev
, renderTiming
, Label
, label
, Timings(..)
, singleton
, lookup
, renderTimings
, reportTimings
, Instant
, Duration(..)
, since
) where

import           Control.Carrier.Time.System
import           Control.Effect.Lift
import           Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import           Data.List (sortOn)
import           Data.Ord (Down(..))
import           Data.Text (Text, pack)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Numeric (showFFloat)
import           Prelude hiding (lookup)
import           System.IO (stderr)

data Timing = Timing
  { total :: !Duration
  , count :: {-# UNPACK #-} !Int
  , min'  :: !Duration
  , max'  :: !Duration
  , sumsq :: !Duration
  , sub   :: !Timings
  }

instance Semigroup Timing where
  Timing t1 c1 mn1 mx1 sq1 sb1 <> Timing t2 c2 mn2 mx2 sq2 sb2 = Timing (t1 + t2) (c1 + c2) (mn1 `min` mn2) (mx1 `max` mx2) sq' (sb1 <> sb2)
    where
    c1' = fromIntegral c1
    c2' = fromIntegral c2
    nom = (t1 * c2' - t2 * c1') ^ (2 :: Int)
    sq' | c1 == 0   = sq2
        | c2 == 0   = sq1
        | otherwise = sq1 + sq2 + nom / ((c1' + c2') * c1' * c2')
  {-# INLINE (<>) #-}

renderTiming :: Timing -> Doc AnsiStyle
renderTiming t@Timing{ total, count, min', max', sub } = table (map go fields) <> if null (unTimings sub) then mempty else nest 2 (line <> renderTimings sub)
    where
    table = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
    fields
      | count == 1 = [ (green "total", prettyMS (realToFrac total)) ]
      | otherwise  =
        [ (green "total", prettyMS (realToFrac total))
        , (green "count", pretty   count)
        , (green "min",   prettyMS (realToFrac min'))
        , (green "mean",  prettyMS (realToFrac (mean t)))
        , (green "max",   prettyMS (realToFrac max'))
        , (green "std.dev.", prettyMS (stdDev t))
        ]
    go (k, v) = k <> colon <+> v
    green = annotate (colorDull Green)
    prettyMS = (<> annotate (colorDull White) "ms") . pretty . ($ "") . showFFloat @Double (Just 3) . (* 1000)

mean :: Timing -> Duration
mean Timing{ total, count }
  | count == 0 = 0
  | otherwise  = total / fromIntegral count
{-# INLINE mean #-}

variance :: Timing -> Duration
variance Timing{ count, sumsq } = sumsq / fromIntegral count
{-# INLINE variance #-}

stdDev :: Timing -> Double
stdDev = sqrt . realToFrac . variance
{-# INLINE stdDev #-}


type Label = Text

label :: String -> Label
label = pack
{-# INLINE label #-}

newtype Timings = Timings { unTimings :: HashMap.HashMap Label Timing }

instance Semigroup Timings where
  Timings t1 <> Timings t2 = Timings (HashMap.unionWith (<>) t1 t2)
  {-# INLINE (<>) #-}

instance Monoid Timings where
  mempty = Timings mempty
  {-# INLINE mempty #-}

singleton :: Label -> Duration -> Timings -> Timings
singleton l t = Timings . HashMap.singleton l . Timing t 1 t t 0
{-# INLINE singleton #-}

lookup :: Label -> Timings -> Maybe Timing
lookup = coerce @(Label -> HashMap.HashMap Label Timing -> _) HashMap.lookup
{-# INLINE lookup #-}

renderTimings :: Timings -> Doc AnsiStyle
renderTimings (Timings ts) = vsep (map go (sortOn (Down . mean . snd) (HashMap.toList ts))) where
  go (k, v) = annotate (color Green) (pretty k) <> pretty ':' <> softline <> renderTiming v

reportTimings :: Has (Lift IO) sig m => Timings -> m ()
reportTimings = sendM . renderIO stderr . layoutPretty defaultLayoutOptions . (<> line) . renderTimings
