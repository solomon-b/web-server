{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.Servant where

--------------------------------------------------------------------------------

import Data.Text.Display (Display (..))
import Servant qualified

--------------------------------------------------------------------------------

instance (Display a) => Display (Servant.Headers x a) where
  displayBuilder Servant.Headers {..} = displayBuilder getResponse

instance Display Servant.NoContent where
  displayBuilder Servant.NoContent = displayBuilder ()
