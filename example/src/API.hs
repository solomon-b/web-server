module API where

--------------------------------------------------------------------------------

import API.About.Get qualified as About.Get
import API.Admin.Blog.Get qualified as Admin.Blog.Get
import API.Admin.Get qualified as Admin.Get
import API.Blog.Delete qualified as Blog.Delete
import API.Blog.Get qualified as Blog.Get
import API.Blog.Id.Delete qualified as Blog.Id.Delete
import API.Blog.Id.Edit.Get qualified as Blog.Id.Edit.Get
import API.Blog.Id.Edit.Post qualified as Blog.Id.Edit.Post
import API.Blog.Id.Get qualified as Blog.Id.Get
import API.Blog.Id.TogglePublish.Patch qualified as Blog.Id.TogglePublish.Patch
import API.Blog.New.Get qualified as Blog.New.Get
import API.Blog.New.Post qualified as Blog.New.Post
import API.Get qualified as Get
import API.Image.Post qualified as Image.Post
import API.MailingList.Post qualified as MailingList.Post
import API.Markdown.Post qualified as Markdown.Post
import API.Static.Get qualified as Static.Get
import API.Store.Delete qualified as Store.Delete
import API.Store.Get qualified as Store.Get
import API.Store.Id.Delete qualified as Store.Id.Delete
import API.Store.Id.Edit.Get qualified as Store.Id.Edit.Get
import API.Store.Id.Edit.Post qualified as Store.Id.Edit.Post
import API.Store.Id.Get qualified as Store.Id.Get
import API.Store.Id.TogglePublish.Patch qualified as Store.Id.TogglePublish.Patch
import API.Store.New.Get qualified as Store.New.Get
import API.Store.New.Post qualified as Store.New.Post
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
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Clock (MonadClock)
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.User qualified as User
import Effects.MailSender (MonadEmail)
import Hasql.Pool qualified as HSQL
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import OpenTelemetry.Trace.Monad (MonadTracer)
import Servant ((:<|>) (..))
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

type API =
  -- Unprotected homepage Routes
  Get.Route
    :<|> Static.Get.Route
    :<|> MailingList.Post.Route
    -- Unprotected BlogPosts Routes
    :<|> Blog.Delete.Route
    :<|> Blog.Get.Route
    :<|> Blog.Id.Get.Route
    :<|> Blog.Id.TogglePublish.Patch.Route
    -- Protected BlogPosts Routes
    :<|> Blog.Id.Delete.Route
    :<|> Blog.Id.Edit.Get.Route
    :<|> Blog.Id.Edit.Post.Route
    :<|> Blog.New.Get.Route
    :<|> Blog.New.Post.Route
    -- Unprotected Store Routes
    :<|> Store.Get.Route
    :<|> Store.Id.Get.Route
    -- Protected Store Routes
    :<|> Store.Delete.Route
    :<|> Store.New.Get.Route
    :<|> Store.New.Post.Route
    :<|> Store.Id.Delete.Route
    :<|> Store.Id.Edit.Get.Route
    :<|> Store.Id.Edit.Post.Route
    :<|> Store.Id.TogglePublish.Patch.Route
    -- Protected Misc Routes
    :<|> Image.Post.Route
    :<|> Markdown.Post.Route
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
    :<|> Admin.Blog.Get.Route

--------------------------------------------------------------------------------

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
    :<|> Blog.Delete.handler
    :<|> Blog.Get.handler
    :<|> Blog.Id.Get.handler
    :<|> Blog.Id.TogglePublish.Patch.handler
    :<|> Blog.Id.Delete.handler
    :<|> Blog.Id.Edit.Get.handler
    :<|> Blog.Id.Edit.Post.handler
    :<|> Blog.New.Get.handler
    :<|> Blog.New.Post.handler
    :<|> Store.Get.handler
    :<|> Store.Id.Get.handler
    :<|> Store.Delete.handler
    :<|> Store.New.Get.handler
    :<|> Store.New.Post.handler
    :<|> Store.Id.Delete.handler
    :<|> Store.Id.Edit.Get.handler
    :<|> Store.Id.Edit.Post.handler
    :<|> Store.Id.TogglePublish.Patch.handler
    :<|> Image.Post.handler
    :<|> Markdown.Post.handler
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
    :<|> Admin.Blog.Get.handler

--------------------------------------------------------------------------------

-- | Route: GET /
rootGetLink :: Links.Link
rootGetLink = Links.safeLink (Proxy @API) (Proxy @Get.Route)

-- | Route: GET /static
staticGetLink :: Links.Link
staticGetLink = Links.safeLink (Proxy @API) (Proxy @Static.Get.Route)

-- | Route: POST /mailing-list
mailingListPostLink :: Links.Link
mailingListPostLink = Links.safeLink (Proxy @API) (Proxy @MailingList.Post.Route)

-- | Route: DELETE /mailing-list
blogDeleteLink :: Links.Link
blogDeleteLink = Links.safeLink (Proxy @API) (Proxy @Blog.Delete.Route)

-- | Route: GET /blog/delete
blogGetLink :: Links.Link
blogGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Get.Route)

-- | Route: GET /blog/:id/delete
blogIdGetLink :: BlogPosts.Id -> Links.Link
blogIdGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Id.Get.Route)

-- | Route: GET /blog/:id/edit
blogIdEditGetLink :: BlogPosts.Id -> Maybe BlogPosts.Body -> Links.Link
blogIdEditGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Id.Edit.Get.Route)

-- | Route: POST /blog/:id/edit
blogIdEditPostLink :: BlogPosts.Id -> Links.Link
blogIdEditPostLink = Links.safeLink (Proxy @API) (Proxy @Blog.Id.Edit.Post.Route)

-- | Route: GET /blog/new
blogNewGetLink :: Links.Link
blogNewGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.New.Get.Route)

-- | Route: POST /blog/new
blogNewPostLink :: Links.Link
blogNewPostLink = Links.safeLink (Proxy @API) (Proxy @Blog.New.Post.Route)

-- | Route: Delete /store
storeDeleteLink :: Links.Link
storeDeleteLink = Links.safeLink (Proxy @API) (Proxy @Store.Delete.Route)

-- | Route: GET /store
storeGetLink :: Links.Link
storeGetLink = Links.safeLink (Proxy @API) (Proxy @Store.Get.Route)

-- | Route: GET /store/new
storeNewGetLink :: Links.Link
storeNewGetLink = Links.safeLink (Proxy @API) (Proxy @Store.New.Get.Route)

-- | Route: POST /store/new
storeNewPostLink :: Links.Link
storeNewPostLink = Links.safeLink (Proxy @API) (Proxy @Store.New.Post.Route)

-- | Route: DELETE /store/:id
storeIdDeleteLink :: Products.Id -> Links.Link
storeIdDeleteLink = Links.safeLink (Proxy @API) (Proxy @Store.Id.Delete.Route)

-- | Route: GET /store/:id/edit
storeIdEditGetLink :: Products.Id -> Maybe Text -> Links.Link
storeIdEditGetLink = Links.safeLink (Proxy @API) (Proxy @Store.Id.Edit.Get.Route)

-- | Route: POST /store/:id/edit
storeIdEditPostLink :: Products.Id -> Links.Link
storeIdEditPostLink = Links.safeLink (Proxy @API) (Proxy @Store.Id.Edit.Post.Route)

-- | Route: GET /store/:id
storeIdGetLink :: Products.Id -> Links.Link
storeIdGetLink = Links.safeLink (Proxy @API) (Proxy @Store.Id.Get.Route)

-- | Route: PATCH /store/:id/toggle-publish
storeIdTogglePublishPatchLink :: Products.Id -> Links.Link
storeIdTogglePublishPatchLink = Links.safeLink (Proxy @API) (Proxy @Store.Id.TogglePublish.Patch.Route)

-- | Route: POST /image
imagePostLink :: Links.Link
imagePostLink = Links.safeLink (Proxy @API) (Proxy @Image.Post.Route)

-- | Route: POST /markdown
markdownPostLink :: Links.Link
markdownPostLink = Links.safeLink (Proxy @API) (Proxy @Markdown.Post.Route)

-- | Route: GET /about
aboutGetLink :: Links.Link
aboutGetLink = Links.safeLink (Proxy @API) (Proxy @About.Get.Route)

-- | Route: GET /user
userGetLink :: Links.Link
userGetLink = Links.safeLink (Proxy @API) (Proxy @User.Get.Route)

-- | Route: GET /user/:id
userIdGetLink :: User.Id -> Links.Link
userIdGetLink = Links.safeLink (Proxy @API) (Proxy @User.Id.Get.Route)

-- | Route: GET /user/register
userRegisterGetLink :: Maybe EmailAddress -> Maybe DisplayName -> Maybe FullName -> Links.Link
userRegisterGetLink = Links.safeLink (Proxy @API) (Proxy @User.Register.Get.Route)

-- | Route: POST /user/register
userRegisterPostLink :: Links.Link
userRegisterPostLink = Links.safeLink (Proxy @API) (Proxy @User.Register.Post.Route)

-- | Route: POST /user/login
userLoginPostLink :: Maybe Text -> Links.Link
userLoginPostLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Post.Route)

-- | Route: GET /user/login
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userLoginGetLink = Links.safeLink (Proxy @API) (Proxy @User.Login.Get.Route)

-- | Route: GET /user/login
userCurrentGetLink :: Links.Link
userCurrentGetLink = Links.safeLink (Proxy @API) (Proxy @User.Current.Get.Route)

-- | Route: GET /user/logout
userLogoutGetLink :: Links.Link
userLogoutGetLink = Links.safeLink (Proxy @API) (Proxy @User.Logout.Get.Route)

-- | Route: POST /user/logout
userLogoutPostLink :: Links.Link
userLogoutPostLink = Links.safeLink (Proxy @API) (Proxy @User.Logout.Post.Route)

-- | Route: DELETE /user/:id/delete
userDeleteLink :: User.Id -> Links.Link
userDeleteLink = Links.safeLink (Proxy @API) (Proxy @User.Delete.Route)

-- | Route: POST /user/:id/password-rest
userPasswordResetPostLink :: User.Id -> Links.Link
userPasswordResetPostLink = Links.safeLink (Proxy @API) (Proxy @User.PasswordReset.Post.Route)

-- | Route: GET /admin
adminGetLink :: Links.Link
adminGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Get.Route)

-- | Route: PATCH /blog/:id/toggle-publish
blogTogglePublish :: BlogPosts.Id -> Links.Link
blogTogglePublish = Links.safeLink (Proxy @API) (Proxy @Blog.Id.TogglePublish.Patch.Route)

-- | Route: GET /admin/blog
adminBlogGetLink :: Links.Link
adminBlogGetLink = Links.safeLink (Proxy @API) (Proxy @Admin.Blog.Get.Route) Nothing Nothing Nothing Nothing

-- | Route: GET /admin/blog?searchQuery=:search
adminBlogGetLinkSearch :: Admin.Blog.Get.SearchQuery -> Links.Link
adminBlogGetLinkSearch search = Links.safeLink (Proxy @API) (Proxy @Admin.Blog.Get.Route) (Just search) Nothing Nothing Nothing
