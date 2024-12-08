{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.UUID where

--------------------------------------------------------------------------------

import Data.Text.Display (Display, ShowInstance (..))
import Data.UUID (UUID)

--------------------------------------------------------------------------------

deriving via (ShowInstance UUID) instance (Display UUID)
