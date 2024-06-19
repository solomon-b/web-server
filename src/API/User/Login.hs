{-# OPTIONS_GHC -Wno-orphans #-}

module API.User.Login where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Text.Internal.Builder qualified as Text
import Deriving.Aeson qualified as Deriving
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User (User)
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User
import Effects.Database.Utils
import Errors (throw401, throw401')
import GHC.Generics (Generic)
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Servant.Auth.Server qualified
import Tracing (handlerSpan)

--------------------------------------------------------------------------------

-- TODO: Move into Orphans Module
instance (Display a) => Display (Servant.Headers x a) where
  displayBuilder :: (Display a) => Servant.Headers x a -> Text.Builder
  displayBuilder Servant.Headers {..} = displayBuilder getResponse

-- TODO: Move into Orphans Module
instance Display Servant.NoContent where
  displayBuilder :: Servant.NoContent -> Text.Builder
  displayBuilder Servant.NoContent = displayBuilder ()

--------------------------------------------------------------------------------

data Login = Login
  { ulEmail :: EmailAddress,
    ulPassword :: Password
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Login)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ul", Deriving.CamelToSnake]] Login

handler ::
  ( MonadReader env m,
    Has Servant.Auth.Server.JWTSettings env,
    Has Servant.Auth.Server.CookieSettings env,
    MonadIO m,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m,
    Has OTEL.Tracer env,
    Display (Servant.Headers [Servant.Header "Set-Cookie" Servant.Auth.Server.SetCookie, Servant.Header "Set-Cookie" Servant.Auth.Server.SetCookie] Servant.NoContent)
  ) =>
  Login ->
  m
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" Servant.Auth.Server.SetCookie,
           Servant.Header "Set-Cookie" Servant.Auth.Server.SetCookie
         ]
        Servant.NoContent
    )
handler req@Login {..} = do
  handlerSpan "/user/login" req display $ do
    cookieSettings <- Reader.asks Has.getter
    jwtSettings <- Reader.asks Has.getter
    execQuerySpanThrowMessage "Failed to query users table" (selectUserByCredentialQuery ulEmail ulPassword) >>= \case
      Just user -> do
        Log.logInfo "Login Attempt" ulEmail
        liftIO (Servant.Auth.Server.acceptLogin cookieSettings jwtSettings (parseModel @_ @User user)) >>= \case
          Nothing -> throw401'
          Just applyCookie -> pure $ applyCookie Servant.NoContent
      Nothing -> do
        Log.logInfo "Invalid Credentials" ulEmail
        throw401 "Invalid Credentials."
