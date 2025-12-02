{-# LANGUAGE ScopedTypeVariables #-}

module API.User.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser, getFullUsers)
import Effects.Observability qualified as Observability
import Log qualified
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "user" :> Servant.Get '[Servant.JSON] [FullUser]

--------------------------------------------------------------------------------

handler ::
  forall m env.
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadReader env m,
    Has Trace.Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  m [FullUser]
handler =
  Observability.handlerSpan "GET /user" $
    (execQuerySpanThrow @_ @_ @env) getFullUsers
