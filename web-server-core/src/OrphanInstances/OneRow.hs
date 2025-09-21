{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.OneRow where

--------------------------------------------------------------------------------

import Data.Text.Display (Display (..))
import Hasql.Interpolate (OneRow (..))

--------------------------------------------------------------------------------

instance (Display a) => Display (OneRow a) where
  displayBuilder = displayBuilder . getOneRow