{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.Timing
( Total(..)
, Timing(..)
, mean
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

newtype Total = Total { getTotal :: Duration }
  deriving (Eq, Ord, Show)

instance Semigroup Total where
  (<>) = coerce ((+) :: Duration -> Duration -> Duration)

instance Monoid Total where
  mempty = Total 0


data Timing = Timing
  { total :: !Total
  , count :: {-# UNPACK #-} !Int
  , min'  :: !Duration
  , max'  :: !Duration
  , sub   :: !Timings
  }

instance Semigroup Timing where
  Timing s1 c1 mn1 mx1 sb1 <> Timing s2 c2 mn2 mx2 sb2 = Timing (s1 <> s2) (c1 + c2) (mn1 `min` mn2) (mx1 `max` mx2) (sb1 <> sb2)
  {-# INLINE (<>) #-}

instance Monoid Timing where
  mempty = Timing mempty 0 0 0 mempty
  {-# INLINE mempty #-}

renderTiming :: Timing -> Doc AnsiStyle
renderTiming t@Timing{ total, count, min', max', sub } = table (map go fields) <> if null (unTimings sub) then mempty else nest 2 (line <> renderTimings sub)
    where
    table = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
    fields
      | count == 1 = [ (green "total", prettyMS (getTotal total)) ]
      | otherwise  =
        [ (green "total", prettyMS (getTotal total))
        , (green "count", pretty   count)
        , (green "min",   prettyMS min')
        , (green "mean",  prettyMS (mean t))
        , (green "max",   prettyMS max')
        ]
    go (k, v) = k <> colon <+> v
    green = annotate (colorDull Green)
    prettyMS = (<> annotate (colorDull White) "ms") . pretty . ($ "") . showFFloat @Double (Just 3) . (* 1000) . realToFrac

mean :: Timing -> Duration
mean Timing{ total, count } = getTotal total / fromIntegral count
{-# INLINE mean #-}


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

singleton :: Label -> Timing -> Timings
singleton = coerce @(Label -> Timing -> _) HashMap.singleton
{-# INLINE singleton #-}

lookup :: Label -> Timings -> Maybe Timing
lookup = coerce @(Label -> HashMap.HashMap Label Timing -> _) HashMap.lookup
{-# INLINE lookup #-}

renderTimings :: Timings -> Doc AnsiStyle
renderTimings (Timings ts) = vsep (map go (sortOn (Down . mean . snd) (HashMap.toList ts))) where
  go (k, v) = annotate (color Green) (pretty k) <> pretty ':' <> softline <> renderTiming v

reportTimings :: Has (Lift IO) sig m => Timings -> m ()
reportTimings = sendM . renderIO stderr . layoutPretty defaultLayoutOptions . (<> line) . renderTimings
