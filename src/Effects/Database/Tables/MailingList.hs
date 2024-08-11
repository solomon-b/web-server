{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.MailingList where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text.Display (Display, RecordInstance (..))
import Domain.Types.Email (EmailAddress)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeRow, EncodeValue, OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------

newtype Id = Id Int64
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the `mailing_list` table.
data Model = Model
  { mId :: Id,
    mEmail :: EmailAddress
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

getEmailListEntries :: Hasql.Statement () [Model]
getEmailListEntries =
  interp
    False
    [sql|
    SELECT id, email
    FROM mailing_list
  |]

newtype ModelInsert = ModelInsert {miEmail :: EmailAddress}
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertEmailAddress :: ModelInsert -> Hasql.Statement () (OneRow Model)
insertEmailAddress ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO mailing_list(email)
    VALUES (#{miEmail})
    RETURNING id, email
  |]
