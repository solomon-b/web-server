module Effects.Database.Queries.User where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User ()
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 ((&&.), (==.))
import Rel8 qualified

--------------------------------------------------------------------------------

selectUserQuery :: User.Id -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserQuery uid = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umId um ==. Rel8.litExpr uid
  pure um

selectUserByCredentialQuery :: EmailAddress -> Password -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserByCredentialQuery (EmailAddress email) (Password pass) = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umEmail um ==. Rel8.litExpr (CI.original email) &&. User.umPassword um ==. Rel8.litExpr pass
  pure um

selectUserByEmailQuery :: EmailAddress -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserByEmailQuery (EmailAddress email) = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umEmail um ==. Rel8.litExpr (CI.original email)
  pure um

selectUsersQuery :: HSQL.Statement () [User.Model Rel8.Result]
selectUsersQuery = Rel8.run . Rel8.select $ Rel8.each User.schema

insertUser ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  (EmailAddress, Password, DisplayName, AdminStatus) ->
  m User.Id
insertUser = execQuerySpanThrowMessage "Failed to insert user" . insertUserQuery

insertUserQuery :: (EmailAddress, Password, DisplayName, AdminStatus) -> HSQL.Statement () User.Id
insertUserQuery newUser =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { into = User.schema,
          rows = Rel8.values [printModel newUser],
          onConflict = Rel8.Abort,
          returning = Rel8.Returning User.umId
        }

deleteUser ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  User.Id ->
  m ()
deleteUser = execQuerySpanThrowMessage "Failed to delete user" . deleteUserQuery

deleteUserQuery :: User.Id -> HSQL.Statement () ()
deleteUserQuery uid =
  Rel8.run_ $
    Rel8.delete $
      Rel8.Delete
        { from = User.schema,
          using = pure (),
          deleteWhere = \_ um -> User.umId um ==. Rel8.litExpr uid,
          returning = Rel8.NoReturning
        }

changeUserPassword ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  User.Id ->
  Password ->
  Password ->
  m (Maybe User.Id)
changeUserPassword uid oldPassword = execQuerySpanThrowMessage "Failed to change user password" . changeUserPasswordQuery uid oldPassword

changeUserPasswordQuery :: User.Id -> Password -> Password -> HSQL.Statement () (Maybe User.Id)
changeUserPasswordQuery uid oldPassword newPassword =
  Rel8.runMaybe $
    Rel8.update $
      Rel8.Update
        { target = User.schema,
          from = pure (),
          set = \_ um -> um {User.umPassword = Rel8.litExpr (coerce newPassword)},
          updateWhere = \_ um -> User.umId um ==. Rel8.litExpr uid &&. User.umPassword um ==. Rel8.litExpr (coerce oldPassword),
          returning = Rel8.Returning User.umId
        }
