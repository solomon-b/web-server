{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.IPRange where

--------------------------------------------------------------------------------

import Data.IP (IPRange)
import Data.Text.Display (Display, ShowInstance (..))

--------------------------------------------------------------------------------

deriving via (ShowInstance IPRange) instance (Display IPRange)
