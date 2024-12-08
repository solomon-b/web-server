{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.Servant where

--------------------------------------------------------------------------------

import Data.Text.Display (Display (..))
import Data.Text.Internal.Builder qualified as Text
import Servant qualified

--------------------------------------------------------------------------------

instance (Display a) => Display (Servant.Headers x a) where
  displayBuilder :: (Display a) => Servant.Headers x a -> Text.Builder
  displayBuilder Servant.Headers {..} = displayBuilder getResponse

instance Display Servant.NoContent where
  displayBuilder :: Servant.NoContent -> Text.Builder
  displayBuilder Servant.NoContent = displayBuilder ()
