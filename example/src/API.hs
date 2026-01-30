module API
  ( API,
    server,
  )
where

import API.Dashboard.Get qualified as Dashboard.Get
import API.Home.Get qualified as Home.Get
import API.User.Login.Get qualified as User.Login.Get
import API.User.Login.Post qualified as User.Login.Post
import API.User.Logout.Post qualified as User.Logout.Post
import API.User.Register.Get qualified as User.Register.Get
import API.User.Register.Post qualified as User.Register.Post
import App.Config (Environment)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.Database.Class (MonadDB)
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant ((:<|>) (..))
import Servant qualified

--------------------------------------------------------------------------------

type API =
  -- Public routes
  Home.Get.Route
    :<|> User.Login.Get.Route
    :<|> User.Login.Post.Route
    :<|> User.Register.Get.Route
    :<|> User.Register.Post.Route
    -- Protected routes
    :<|> Dashboard.Get.Route
    :<|> User.Logout.Post.Route

--------------------------------------------------------------------------------

server ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Log.MonadLog m,
    MonadDB m,
    MonadIO m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Environment ->
  Servant.ServerT API m
server _env =
  Home.Get.handler
    :<|> User.Login.Get.handler
    :<|> User.Login.Post.handler
    :<|> User.Register.Get.handler
    :<|> User.Register.Post.handler
    :<|> Dashboard.Get.handler
    :<|> User.Logout.Post.handler
