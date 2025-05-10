module Test.Gen.Tables.Images where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

imageInsertGen :: (MonadIO m, MonadGen m) => m (Text, Text)
imageInsertGen = do
  iTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  miFilePath <- Gen.text (Range.linear 1 100) Gen.alphaNum
  pure (iTitle, miFilePath)

imageUpdateGen :: (MonadIO m, MonadGen m) => m (Text, Text)
imageUpdateGen = do
  muTitle <- Gen.text (Range.linear 1 10) Gen.alphaNum
  muFilePath <- Gen.text (Range.linear 1 100) Gen.alphaNum
  pure (muTitle, muFilePath)
