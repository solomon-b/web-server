{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Blog.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminBlogGetLink, blogIdEditGetLink, blogNewGetLink)
import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav, loadFrameWithNavAdmin)
import Control.Monad (forM, forM_)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (Day)
import Domain.Types.DisplayName (DisplayName, mkDisplayNameUnsafe)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Log qualified
import Lucid (action_, button_, class_, div_, form_, h1_, id_, input_, method_, name_, option_, placeholder_, script_, select_, span_, table_, tbody_, td_, th_, thead_, tr_, type_, value_)
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.Extras (hxGet_, hxInclude_, hxParams_, hxPost_, hxPushUrl_, hxSwap_, hxTarget_, hxTrigger_)
import OpenTelemetry.Trace.Core qualified as Trace
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Link
import Text.Fuzzy qualified as Fuzzy
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> Servant.QueryParam "searchQuery" SearchQuery
    :> Servant.QueryParam "startDate" StartDate
    :> Servant.QueryParam "endDate" EndDate
    :> Servant.QueryParam "author" DisplayName
    :> "admin"
    :> "blog"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))

newtype SearchQuery = SearchQuery {searchQuery :: Text}
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData)

newtype StartDate = StartDate {getStartDate :: Maybe Day}
  deriving newtype (Servant.ToHttpApiData)

instance Servant.FromHttpApiData StartDate where
  parseQueryParam :: Text -> Either Text StartDate
  parseQueryParam "" = Right $ StartDate Nothing
  parseQueryParam qf = StartDate . Just <$> Servant.parseQueryParam qf

newtype EndDate = EndDate {getEndDate :: Maybe Day}
  deriving newtype (Servant.ToHttpApiData)

instance Servant.FromHttpApiData EndDate where
  parseQueryParam :: Text -> Either Text EndDate
  parseQueryParam "" = Right $ EndDate Nothing
  parseQueryParam qf = EndDate . Just <$> Servant.parseQueryParam qf

--------------------------------------------------------------------------------

template :: [(BlogPosts.Domain, Maybe User.Domain)] -> Lucid.Html ()
template posts = div_ [class_ "p-6", makeAttributes "x-data" "{ selectAll: false }"] $ do
  h1_ [class_ "mt-1 mb-1 text-3xl tracking-tight text-slate-900"] "Posts"

  -- Toolbar
  div_ [class_ "flex flex-wrap justify-between items-center mb-4 gap-4"] $ do
    -- Left: Filters
    div_ [class_ "flex items-center space-x-2"] $ do
      form_
        [ -- hxGet_ "/admin/blog",
          -- hxTarget_ "#admin-main",
          -- hxTrigger_ "change from:form",
          -- hxParams_ "*",
          -- hxInclude_ "closest form",
          action_ "/admin/blog",
          method_ "GET"
        ]
        $ do
          input_
            [ id_ "search",
              type_ "text",
              name_ "searchQuery",
              value_ "",
              placeholder_ "Search posts...",
              class_ "mr-2 px-3 py-2 border rounded-md text-sm focus:ring-green-500 focus:border-green-500"
            ]

          select_
            [ name_ "author",
              class_ "mr-2 px-3 py-2 border rounded-md text-sm"
            ]
            $ do
              option_ [value_ ""] "All Authors"
              let authors = nub $ User.dDisplayName <$> mapMaybe snd posts
              forM_ authors $ \author ->
                option_ [value_ (display author)] $ Lucid.toHtml author

          input_
            [ class_ "mr-2 px-3 py-2 border rounded-md text-sm",
              type_ "date",
              name_ "startDate",
              placeholder_ "Start date"
            ]

          span_ [class_ "mr-2 text-sm text-gray-500"] "to"

          input_
            [ class_ "mr-2 px-3 py-2 border rounded-md text-sm",
              type_ "date",
              name_ "endDate",
              placeholder_ "End date"
            ]

          button_
            [ type_ "submit",
              class_ "px-4 py-2 bg-gray-200 text-sm rounded hover:bg-gray-300"
            ]
            "Submit"

    -- Right: Action Buttons
    div_ [class_ "flex space-x-2"] $ do
      button_
        [ class_ "px-4 py-2 bg-green-600 text-white text-sm rounded hover:bg-green-700",
          hxGet_ [i|/#{blogNewUrl}|],
          hxTarget_ "#admin-main",
          hxPushUrl_ "true"
        ]
        "New Post"
      button_
        [ hxPost_ "/blog/delete",
          hxInclude_ ".row-checkbox:checked",
          hxTarget_ "#admin-main",
          hxSwap_ "outerHTML",
          class_ "px-4 py-2 bg-gray-200 text-sm rounded hover:bg-gray-300"
        ]
        "Delete Selected"

  -- Posts Table
  div_ [id_ "post-table", class_ "overflow-x-auto"] $
    table_ [class_ "min-w-full text-sm text-left text-gray-700 border border-gray-200 rounded-lg"] $ do
      -- Table Header
      thead_ [class_ "bg-gray-50 text-xs uppercase text-gray-500"] $
        tr_ $ do
          th_ [class_ "px-4 py-3"] $
            input_
              [ type_ "checkbox",
                makeAttributes "x-model" "selectAll",
                makeAttributes "@change" "document.querySelectorAll('.row-checkbox').forEach(cb => cb.checked = $event.target.checked)"
              ]
          th_ [class_ "px-4 py-3"] $ button_ [class_ "hover:underline"] "Title"
          th_ [class_ "px-4 py-3"] $ button_ [class_ "hover:underline"] "Author"
          th_ [class_ "px-4 py-3"] $ button_ [class_ "hover:underline"] "Date"

      -- Table Body
      tbody_ $ do
        forM_ posts $ \(BlogPosts.Domain {..}, user) -> do
          tr_ [class_ "border-t hover:bg-gray-50"] $ do
            td_ [class_ "px-4 py-3"] $ input_ [type_ "checkbox", name_ "ids", class_ "row-checkbox", value_ (display dId)]
            td_ [class_ "px-4 py-3 font-medium text-gray-900"] $ do
              button_ [hxGet_ [i|/#{blogEditUrl dId}|], hxTarget_ "#admin-main", hxPushUrl_ "true"] $
                Lucid.toHtml $
                  display dTitle
            td_ [class_ "px-4 py-3"] $ Lucid.toHtml $ display $ maybe (mkDisplayNameUnsafe "null") User.dDisplayName user
            td_ [class_ "px-4 py-3"] "2025-03-31"

blogNewUrl :: Link.URI
blogNewUrl = Link.linkURI blogNewGetLink

blogEditUrl :: BlogPosts.Id -> Link.URI
blogEditUrl = Link.linkURI . flip blogIdEditGetLink Nothing

adminBlogGetUrl :: Link.URI
adminBlogGetUrl = Link.linkURI adminBlogGetLink

--------------------------------------------------------------------------------

unauthorized :: Lucid.Html ()
unauthorized =
  h1_ [class_ "mt-1 text-xl font-extrabold tracking-tight text-slate-900"] "Unauthorized"

--------------------------------------------------------------------------------
-- Handler

handler ::
  forall m env.
  ( Has Trace.Tracer env,
    Log.MonadLog m,
    MonadCatch m,
    MonadDB m,
    MonadReader env m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadIO m
  ) =>
  Auth.Authz ->
  Maybe Bool ->
  Maybe SearchQuery ->
  Maybe StartDate ->
  Maybe EndDate ->
  Maybe DisplayName ->
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler (Auth.Authz user@User.Domain {..} _) hxTrigger searchQuery startDate endDate author = do
  Observability.handlerSpan "GET /admin/blog" () (display . Servant.getResponse) $
    if dIsAdmin
      then do
        posts <- maybe id filterPosts searchQuery . fmap BlogPosts.toDomain <$> execQuerySpanThrow BlogPosts.getBlogPosts
        postsWithAuthors <- fmap (maybe id filterByAuthor author) <$> forM posts $ \post -> do
          postAuthor <- fmap (fmap User.toDomain) <$> execQuerySpanThrow $ User.getUser (BlogPosts.dAuthorId post)
          pure (post, postAuthor)

        let page = template postsWithAuthors
        case hxTrigger of
          Just True ->
            pure $ Servant.addHeader "HX-Request" page
          _ -> do
            pageWithFrame <- loadFrameWithNavAdmin (Auth.IsLoggedIn user) "about-tab" page
            pure $ Servant.addHeader "HX-Request" pageWithFrame
      else renderUnauthorized $ Auth.IsLoggedIn user

renderUnauthorized :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Auth.LoggedIn -> m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
renderUnauthorized loginState = do
  page <- loadFrameWithNav loginState "about-tab" unauthorized
  pure $ Servant.addHeader "HX-Request" page

filterByAuthor :: DisplayName -> [(BlogPosts.Domain, Maybe User.Domain)] -> [(BlogPosts.Domain, Maybe User.Domain)]
filterByAuthor name = filter (maybe False ((== name) . User.dDisplayName) . snd)

filterPosts :: SearchQuery -> [BlogPosts.Domain] -> [BlogPosts.Domain]
filterPosts (SearchQuery q) xs =
  Fuzzy.original <$> Fuzzy.filter q xs "" "" (\p -> mconcat [BlogPosts.getSubject (BlogPosts.dTitle p), BlogPosts.getBody (BlogPosts.dContent p)]) False
