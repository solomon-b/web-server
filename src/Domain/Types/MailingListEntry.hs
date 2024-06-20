{-# OPTIONS_GHC -Wno-orphans #-}
module Domain.Types.MailingListEntry where

--------------------------------------------------------------------------------

import Barbies
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Text.Display (Display (..), RecordInstance (..))
import Domain.Types.Email
import Effects.Database.Utils
import GHC.Generics
import Rel8 qualified
import Servant.Auth.JWT (FromJWT, ToJWT)
import qualified Effects.Database.Tables.MailingList as MailingList

--------------------------------------------------------------------------------
-- Domain

data MailingListEntry = MailingListEntry
  { mlId :: MailingList.Id,
    mlEmail :: EmailAddress
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance MailingListEntry)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

instance ModelParser MailingList.Model MailingListEntry where
  parseModel :: MailingList.Model Rel8.Result -> MailingListEntry
  parseModel = parser . runIdentity . bsequence'
    where
      parser :: MailingList.Model Identity -> MailingListEntry
      parser MailingList.Model {..} =
        MailingListEntry
          { mlId = coerce mlId,
            mlEmail = EmailAddress (CI.mk (runIdentity mlEmail))
          }

instance ModelPrinter MailingList.Model EmailAddress where
  printModel :: EmailAddress -> MailingList.Model Rel8.Expr
  printModel email =
    MailingList.Model
      { mlId = Rel8.unsafeDefault,
        mlEmail = Rel8.litExpr (CI.original (coerce email))
      }
