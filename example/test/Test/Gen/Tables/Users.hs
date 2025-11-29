module Test.Gen.Tables.Users where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.DisplayName (genDisplayName)
import Test.Gen.EmailAddress (genEmail)
import Test.Gen.FullName (genFullName)
import Test.Gen.Password (genPassword)

--------------------------------------------------------------------------------

userInsertGen :: (MonadIO m, MonadGen m) => m User.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miPassword <- genPassword
  pure User.ModelInsert {..}

-- | Generator for user metadata
-- Note: This requires a User.Id, so it should be used after inserting a user
userMetadataInsertGen :: (MonadIO m, MonadGen m) => User.Id -> m UserMetadata.ModelInsert
userMetadataInsertGen userId = do
  let miUserId = userId
  miDisplayName <- genDisplayName
  miFullName <- genFullName
  miAvatarUrl <- Gen.maybe $ Gen.text (Range.linear 1 10) Gen.alphaNum
  miIsAdmin <- Gen.bool
  pure UserMetadata.ModelInsert {..}
