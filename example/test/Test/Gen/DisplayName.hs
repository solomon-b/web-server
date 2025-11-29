module Test.Gen.DisplayName where

--------------------------------------------------------------------------------

import Domain.Types.DisplayName
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genDisplayName :: (MonadGen m) => m DisplayName
genDisplayName = Gen.justT $ do
  displayName <- Gen.text (Range.linear 1 10) Gen.alphaNum
  pure $ mkDisplayName displayName
