{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.User where

--------------------------------------------------------------------------------

import Barbies
import Control.Applicative (Const (..))
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, display)
import Effects.FormBuilder (FormBuilder (..), ToForm (..))
import GHC.Generics
import Lucid qualified
import Rel8 qualified
import Servant qualified
import Servant.Auth.JWT (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, Servant.FromHttpApiData, Rel8.DBEq, Rel8.DBType, ToJSON, FromJSON, Display)
  deriving anyclass (ToJWT, FromJWT)

-- | Database Model for the `user` table.
data Model f = Model
  { umId :: f Id,
    umEmail :: f Text,
    umPassword :: f Text,
    umDisplayName :: f Text,
    umAvatarUrl :: f (Maybe Text),
    umIsAdmin :: f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8.Rel8able, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

schema :: Rel8.TableSchema (Model Rel8.Name)
schema =
  Rel8.TableSchema
    { Rel8.name = "users",
      Rel8.columns =
        Model
          { umId = "id",
            umEmail = "email",
            umPassword = "password",
            umDisplayName = "display_name",
            umAvatarUrl = "avatar_url",
            umIsAdmin = "is_admin"
          }
    }

instance FormBuilder Model where
  type Index Model = Id
  type Name Model = Text

  routeName :: Text
  routeName = "user"

  idColumn = runIdentity . umId

  rowForm :: (Monad m) => Model (ToForm m)
  rowForm =
    Model
      { umId = ToForm $ \x -> Lucid.td_ (fromString $ Text.unpack $ display x),
        umEmail = ToForm $ \x -> Lucid.td_ (fromString $ Text.unpack $ display x),
        umPassword = ToForm $ \x -> Lucid.td_ (fromString $ Text.unpack $ display x),
        umDisplayName = ToForm $ \x -> Lucid.td_ (fromString $ Text.unpack $ display x),
        umAvatarUrl = ToForm $ \x -> Lucid.td_ (fromString $ Text.unpack $ display x),
        umIsAdmin = ToForm $ \x -> Lucid.td_ (fromString $ Text.unpack $ display x)
      }

  columnNames :: (Monad m) => Model (Const (Lucid.HtmlT m ()))
  columnNames =
    Model
      { umId = Const "id",
        umEmail = Const "email",
        umPassword = Const "password",
        umDisplayName = Const "displayName",
        umAvatarUrl = Const "avatarUrl",
        umIsAdmin = Const "isAdmin"
      }
