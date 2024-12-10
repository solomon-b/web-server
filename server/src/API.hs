module API where

{-

- [-] Blog System
  - [X] Table Schema / Model
  - [ ] Index Page
    - [ ] Filtering/Ordering
    - [ ] Search
  - [ ] Post Page
  - [ ] Editor Page
  - [ ] Markdown
    - [ ] Parsing
    - [ ] Printing
  - [ ] Auth
  - [ ] RSS
-}

--------------------------------------------------------------------------------

import API.About.Get qualified as About.Get
import API.Admin.Get qualified as Admin.Get
import API.Blog.Get qualified as Blog.Get
import API.Blog.Id.Edit.Get qualified as Blog.Id.Edit.Get
import API.Blog.Id.Edit.Post qualified as Blog.Id.Edit.Post
import API.Blog.Id.Get qualified as Blog.Id.Get
import API.Blog.Id.Preview.Get qualified as Blog.Id.Preview.Get
import API.Blog.New.Edit.Get qualified as Blog.New.Edit.Get
import API.Blog.New.Get qualified as Blog.New.Get
import API.Blog.New.Post qualified as Blog.New.Post
import API.Blog.New.Preview.Get qualified as Blog.New.Preview.Get
import API.Get qualified as Get
import API.Image.Post qualified as Image.Post
import API.MailingList.Post qualified as MailingList.Post
import API.Static.Get qualified as Static.Get
import API.User.Current.Get qualified as User.Current.Get
import API.User.Delete qualified as Delete
import API.User.Delete qualified as User.Delete
import API.User.Get qualified as User.Get
import API.User.Id.Get qualified as User.Id.Get
import API.User.Login.Get qualified as User.Login.Get
import API.User.Login.Post qualified as User.Login.Post
import API.User.Logout.Get qualified as User.Logout.Get
import API.User.Logout.Post qualified as User.Logout.Post
import API.User.PasswordReset.Post qualified as PasswordReset.Post
import API.User.PasswordReset.Post qualified as User.PasswordReset.Post
import API.User.Register.Get qualified as User.Register.Get
import API.User.Register.Post qualified as User.Register.Post
import App.Config (Environment, Hostname, SmtpConfig)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.MailSender (MonadEmail)
import Hasql.Pool qualified as HSQL
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Monad (MonadTracer)
import Servant ((:<|>) (..))
import Servant qualified

--------------------------------------------------------------------------------

type API =
  -- Unprotected homepage Routes
  Get.Route
    :<|> Static.Get.Route
    :<|> MailingList.Post.Route
    -- Unprotected BlogPosts Routes
    :<|> Blog.Get.Route
    :<|> Blog.Id.Get.Route
    -- Protected BlogPosts Routes
    :<|> Blog.Id.Edit.Get.Route
    :<|> Blog.Id.Edit.Post.Route
    :<|> Blog.Id.Preview.Get.Route
    :<|> Blog.New.Get.Route
    :<|> Blog.New.Post.Route
    :<|> Blog.New.Edit.Get.Route
    :<|> Blog.New.Preview.Get.Route
    -- Protected Image Routes
    :<|> Image.Post.Route
    -- Unprotected State Page Routes
    :<|> About.Get.Route
    -- Unprotected User Routes
    :<|> User.Get.Route
    :<|> User.Id.Get.Route
    :<|> User.Register.Get.Route
    :<|> User.Register.Post.Route
    :<|> User.Login.Post.Route
    :<|> User.Login.Get.Route
    -- Protected User Routes
    :<|> User.Current.Get.Route
    :<|> User.Logout.Get.Route
    :<|> User.Logout.Post.Route
    :<|> User.Delete.Route
    :<|> User.PasswordReset.Post.Route
    -- Protected Admin Routes
    :<|> Admin.Get.Route

server ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has OTEL.Tracer env,
    Has (Maybe SmtpConfig) env,
    Has Hostname env,
    Has Environment env,
    Log.MonadLog m,
    MonadClock m,
    MonadDB m,
    MonadEmail m,
    MonadIO m,
    MonadTracer m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Environment ->
  Servant.ServerT API m
server env = do
  Get.handler
    :<|> Static.Get.handler env
    :<|> MailingList.Post.handler
    :<|> Blog.Get.handler
    :<|> Blog.Id.Get.handler
    :<|> Blog.Id.Edit.Get.handler
    :<|> Blog.Id.Edit.Post.handler
    :<|> Blog.Id.Preview.Get.handler
    :<|> Blog.New.Get.handler
    :<|> Blog.New.Post.handler
    :<|> Blog.New.Edit.Get.handler
    :<|> Blog.New.Preview.Get.handler
    :<|> Image.Post.handler
    :<|> About.Get.handler
    :<|> User.Get.handler
    :<|> User.Id.Get.handler
    :<|> User.Register.Get.handler
    :<|> User.Register.Post.handler
    :<|> User.Login.Post.handler
    :<|> User.Login.Get.handler
    :<|> User.Current.Get.handler
    :<|> User.Logout.Get.handler
    :<|> User.Logout.Post.handler
    :<|> Delete.handler
    :<|> PasswordReset.Post.handler
    :<|> Admin.Get.handler
