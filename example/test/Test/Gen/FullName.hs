module Test.Gen.FullName where

--------------------------------------------------------------------------------

import Domain.Types.FullName
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genFullName :: (MonadGen m) => m FullName
genFullName = Gen.justT $ do
  fullName <- Gen.text (Range.linear 1 10) Gen.alphaNum
  pure $ mkFullName fullName
