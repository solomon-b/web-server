{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.User where

--------------------------------------------------------------------------------

import Barbies
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import GHC.Generics
import OrphanInstances ()
import Rel8 qualified

--------------------------------------------------------------------------------
-- Domain

data User = User
  { userId :: User.Id,
    userEmail :: EmailAddress,
    userDisplayName :: DisplayName,
    userAvatarUrl :: Maybe Text,
    userIsAdmin :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance User)
  deriving anyclass (FromJSON, ToJSON)

instance ModelParser User.Model User where
  parseModel :: User.Model Rel8.Result -> User
  parseModel = parser . runIdentity . bsequence'
    where
      parser :: User.Model Identity -> User
      parser User.Model {..} =
        User
          { userId = coerce umId,
            userEmail = EmailAddress (CI.mk (runIdentity umEmail)),
            userDisplayName = coerce umDisplayName,
            userAvatarUrl = coerce umAvatarUrl,
            userIsAdmin = coerce umIsAdmin
          }

instance ModelPrinter User.Model (EmailAddress, PasswordHash Argon2, DisplayName, AdminStatus) where
  printModel :: (EmailAddress, PasswordHash Argon2, DisplayName, AdminStatus) -> User.Model Rel8.Expr
  printModel (email, pass, DisplayName displayName, adminStatus) =
    User.Model
      { umId = Rel8.unsafeDefault,
        umEmail = Rel8.litExpr (CI.original (coerce email)),
        umPassword = Rel8.litExpr pass,
        umDisplayName = Rel8.litExpr displayName,
        umAvatarUrl = Rel8.litExpr Nothing,
        umIsAdmin = Rel8.litExpr $ isAdmin adminStatus
      }
