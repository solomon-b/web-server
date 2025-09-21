module Test.Gen.Tables.Users where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.User qualified as UUT
import Hedgehog (MonadGen (..))
import Test.Gen.EmailAddress (genEmail)
import Test.Gen.Password (genPassword)

--------------------------------------------------------------------------------

userInsertGen :: (MonadIO m, MonadGen m) => m UUT.ModelInsert
userInsertGen = do
  miEmail <- genEmail
  miPassword <- genPassword
  pure UUT.ModelInsert {..}
