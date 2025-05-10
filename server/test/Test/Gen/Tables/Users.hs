module Test.Gen.Tables.Users where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.User qualified as UUT
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.DisplayName (genDisplayName)
import Test.Gen.EmailAddress (genEmail)
import Test.Gen.FullName (genFullName)
import Test.Gen.Password (genPassword)

--------------------------------------------------------------------------------

userInsertGen :: (MonadIO m, MonadGen m) => m UUT.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miDisplayName <- genDisplayName
  miFullName <- genFullName
  miPassword <- genPassword
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure UUT.ModelInsert {..}
