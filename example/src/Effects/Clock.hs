module Effects.Clock
  ( MonadClock (..),
    UTCTime (..),
    Clock.addUTCTime,
    Clock.nominalDay,
  )
where

--------------------------------------------------------------------------------

import Data.Time (UTCTime)
import Data.Time.Clock qualified as Clock

--------------------------------------------------------------------------------

class MonadClock m where
  currentSystemTime :: m UTCTime
