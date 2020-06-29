{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.Timing
( Timing(..)
, mean
, renderTiming
, Label
, label
, Timings(..)
, singleton
, lookup
, renderTimings
, reportTimings
, Duration(..)
, now
) where

import           Control.Effect.Lift
import           Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import           Data.List (sortOn)
import           Data.Ord (Down(..))
import           Data.Text (Text, pack)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Numeric (showFFloat)
import           Prelude hiding (lookup)
import           System.IO (stderr)

data Timing = Timing
  { sum   :: !NominalDiffTime
  , min'  :: !NominalDiffTime
  , max'  :: !NominalDiffTime
  , count :: {-# UNPACK #-} !Int
  , sub   :: !Timings
  }

instance Semigroup Timing where
  Timing s1 mn1 mx1 c1 sb1 <> Timing s2 mn2 mx2 c2 sb2 = Timing (s1 + s2) (mn1 `min` mn2) (mx1 `max` mx2) (c1 + c2) (sb1 <> sb2)
  {-# INLINE (<>) #-}

instance Monoid Timing where
  mempty = Timing 0 0 0 0 mempty
  {-# INLINE mempty #-}

renderTiming :: Timing -> Doc AnsiStyle
renderTiming t@Timing{ min', max', sub } = table (map go fields) <> if null (unTimings sub) then mempty else nest 2 (line <> renderTimings sub)
    where
    table = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
    fields =
      [ (annotate (colorDull Green) "min", prettyMS min')
      , (annotate (colorDull Green) "mean", prettyMS (mean t))
      , (annotate (colorDull Green) "max", prettyMS max')
      ]
    go (k, v) = k <> colon <+> v
    prettyMS = (<> annotate (colorDull White) "ms") . pretty . ($ "") . showFFloat @Double (Just 3) . (* 1000) . realToFrac

mean :: Timing -> NominalDiffTime
mean Timing{ sum, count } = sum / fromIntegral count
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


newtype Duration = Duration { getDuration :: SystemTime }

instance Semigroup Duration where
  Duration (MkSystemTime s1 ns1) <> Duration (MkSystemTime s2 ns2) = Duration (MkSystemTime (s1 + s2) (ns1 + ns2))

instance Monoid Duration where
  mempty = Duration (MkSystemTime 0 0)

now :: Has (Lift IO) sig m => m Duration
now = Duration <$> sendIO getSystemTime
