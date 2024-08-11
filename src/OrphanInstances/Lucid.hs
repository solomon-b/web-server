{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.Lucid where

--------------------------------------------------------------------------------

import Data.Text.Display (Display, ShowInstance (..))
import Lucid.Base qualified

--------------------------------------------------------------------------------

deriving via (ShowInstance (Lucid.Base.Html ())) instance (Display (Lucid.Base.Html ()))
