module Effects.Database.Queries.User where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Data.CaseInsensitive qualified as CI
import Data.Password.Argon2 (Argon2, PasswordHash)
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.User (User)
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 ((&&.), (==.))
import Rel8 qualified

--------------------------------------------------------------------------------

selectUser ::
  ( Log.MonadLog f,
    MonadDB f,
    MonadThrow f
  ) =>
  User.Id ->
  f (Maybe User)
selectUser uid =
  execQuerySpanThrowMessage' "Failed to query users table" (selectUserQuery uid)

selectUserQuery :: User.Id -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserQuery uid = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umId um ==. Rel8.litExpr uid
  pure um

--------------------------------------------------------------------------------

selectUserByCredential ::
  ( Log.MonadLog f,
    MonadDB f,
    MonadThrow f
  ) =>
  EmailAddress ->
  PasswordHash Argon2 ->
  f (Maybe User)
selectUserByCredential email password =
  execQuerySpanThrowMessage' "Failed to query users table" (selectUserByCredentialQuery email password)

selectUserByCredentialQuery :: EmailAddress -> PasswordHash Argon2 -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserByCredentialQuery (EmailAddress email) pass = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umEmail um ==. Rel8.litExpr (CI.original email) &&. User.umPassword um ==. Rel8.litExpr pass
  pure um

--------------------------------------------------------------------------------

selectUserByEmail ::
  ( Log.MonadLog f,
    MonadDB f,
    MonadThrow f
  ) =>
  EmailAddress ->
  f (Maybe (User.Model Rel8.Result))
selectUserByEmail email =
  execQuerySpanThrowMessage "Failed to query users table" (selectUserByEmailQuery email)

selectUserByEmailQuery :: EmailAddress -> HSQL.Statement () (Maybe (User.Model Rel8.Result))
selectUserByEmailQuery (EmailAddress email) = Rel8.runMaybe . Rel8.select $ do
  um <- Rel8.each User.schema
  Rel8.where_ $ User.umEmail um ==. Rel8.litExpr (CI.original email)
  pure um

--------------------------------------------------------------------------------

selectUsers ::
  ( Log.MonadLog f,
    MonadDB f,
    MonadThrow f
  ) =>
  f [User]
selectUsers =
  execQuerySpanThrowMessage' "Failed to query users table" selectUsersQuery

selectUsersQuery :: HSQL.Statement () [User.Model Rel8.Result]
selectUsersQuery = Rel8.run . Rel8.select $ Rel8.each User.schema

--------------------------------------------------------------------------------

insertUser ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  (EmailAddress, PasswordHash Argon2, DisplayName, AdminStatus) ->
  m User.Id
insertUser = execQuerySpanThrowMessage "Failed to insert user" . insertUserQuery

insertUserQuery :: (EmailAddress, PasswordHash Argon2, DisplayName, AdminStatus) -> HSQL.Statement () User.Id
insertUserQuery newUser =
  Rel8.run1 $
    Rel8.insert $
      Rel8.Insert
        { into = User.schema,
          rows = Rel8.values [printModel newUser],
          onConflict = Rel8.Abort,
          returning = Rel8.Returning User.umId
        }

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

changeUserPassword ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  User.Id ->
  PasswordHash Argon2 ->
  PasswordHash Argon2 ->
  m (Maybe User.Id)
changeUserPassword uid oldPassword = execQuerySpanThrowMessage "Failed to change user password" . changeUserPasswordQuery uid oldPassword

changeUserPasswordQuery :: User.Id -> PasswordHash Argon2 -> PasswordHash Argon2 -> HSQL.Statement () (Maybe User.Id)
changeUserPasswordQuery uid oldPassword newPassword =
  Rel8.runMaybe $
    Rel8.update $
      Rel8.Update
        { target = User.schema,
          from = pure (),
          set = \_ um -> um {User.umPassword = Rel8.litExpr newPassword},
          updateWhere = \_ um -> User.umId um ==. Rel8.litExpr uid &&. User.umPassword um ==. Rel8.litExpr oldPassword,
          returning = Rel8.Returning User.umId
        }
