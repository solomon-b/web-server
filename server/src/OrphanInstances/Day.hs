{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.Day where

--------------------------------------------------------------------------------

import Data.Text.Display (Display, ShowInstance (..))
import Data.Time (Day)

--------------------------------------------------------------------------------

deriving via (ShowInstance Day) instance (Display Day)
