{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.UTCTime where

--------------------------------------------------------------------------------

import Data.Text.Display (Display, ShowInstance (..))
import Data.Time (UTCTime)

--------------------------------------------------------------------------------

deriving via (ShowInstance UTCTime) instance (Display UTCTime)
