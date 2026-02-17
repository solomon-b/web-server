module Effects.Clock
  ( MonadClock (..),
    UTCTime (..),
    Clock.addUTCTime,
    Clock.nominalDay,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Time (UTCTime)
import Data.Time.Clock qualified as Clock

--------------------------------------------------------------------------------

class MonadClock m where
  currentSystemTime :: m UTCTime

instance (MonadClock m, Monad m) => MonadClock (ExceptT e m) where
  currentSystemTime = lift currentSystemTime
