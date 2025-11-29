{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Blog.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminBlogGetLink, blogIdEditGetLink, blogNewGetLink, blogTogglePublish)
import App.Auth qualified as Auth
import Component.Frame (loadFrameWithNav, loadFrameWithNavAdmin)
import Control.Monad (forM, forM_, join)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Has (Has)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Monoid (All (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (Display, display)
import Data.Time (Day, utctDay)
import Domain.Types.DisplayName (DisplayName, mkDisplayName, mkDisplayNameUnsafe)
import Domain.Types.OptionalField (OptionalField, flatten)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Queries.UserWithMetadata (FullUser (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Observability qualified as Observability
import Log qualified
import Lucid (action_, br_, button_, checked_, class_, div_, form_, h1_, id_, input_, label_, method_, name_, option_, placeholder_, script_, select_, selected_, span_, table_, tbody_, td_, th_, thead_, tr_, type_, value_)
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.Extras (hxFollowDirects_, hxGet_, hxInclude_, hxParams_, hxPatch_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import OpenTelemetry.Trace.Core qualified as Trace
import OrphanInstances.Day
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Link
import Servant.Multipart qualified as Servant
import Text.Fuzzy qualified as Fuzzy
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> Servant.Header "HX-Request" Bool
    :> Servant.QueryParam "searchQuery" SearchQuery
    :> Servant.QueryParam "startDate" (OptionalField StartDate)
    :> Servant.QueryParam "endDate" (OptionalField EndDate)
    :> Servant.QueryParam "author" (OptionalField DisplayName)
    :> "admin"
    :> "blog"
    :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))

newtype SearchQuery = SearchQuery {searchQuery :: Text}
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, Display)

newtype StartDate = StartDate {getStartDate :: Day}
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, Display)

newtype EndDate = EndDate {getEndDate :: Day}
  deriving newtype (Servant.ToHttpApiData, Servant.FromHttpApiData, Display)

data SearchForm = SearchForm
  { searchField :: Maybe SearchQuery,
    authorField :: Maybe DisplayName,
    startDateField :: Maybe StartDate,
    endDateField :: Maybe EndDate,
    published :: Maybe Bool
  }

--------------------------------------------------------------------------------

template :: [(BlogPosts.Domain, FullUser)] -> SearchForm -> Lucid.Html ()
template posts SearchForm {..} = div_ [class_ "p-6", makeAttributes "x-data" "{ selectAll: false }"] $ do
  h1_ [class_ "mt-1 mb-1 text-3xl tracking-tight text-slate-900"] "Posts"

  -- Toolbar
  div_ [class_ "flex flex-wrap justify-between items-center mb-4 gap-4"] $ do
    -- Left: Filters
    div_ [class_ "flex items-center space-x-2"] $ do
      form_
        [ hxGet_ "/admin/blog",
          hxTarget_ "#admin-main",
          hxTrigger_ "change from:form",
          hxParams_ "*",
          hxInclude_ "closest form",
          action_ "/admin/blog",
          method_ "GET"
        ]
        $ do
          input_
            [ id_ "search",
              type_ "text",
              name_ "searchQuery",
              value_ (maybe "" display searchField),
              placeholder_ "Search posts...",
              class_ "mr-2 px-3 py-2 border rounded-md text-sm focus:ring-green-500 focus:border-green-500"
            ]

          select_
            [ name_ "author",
              class_ "mr-2 px-3 py-2 border rounded-md text-sm"
            ]
            $ do
              option_ [value_ ""] "All Authors"
              let authors = nub $ fuDisplayName . snd <$> posts
              forM_ authors $ \author ->
                if Just author == authorField
                  then option_ [value_ (display author), selected_ ""] $ Lucid.toHtml author
                  else option_ [value_ (display author)] $ Lucid.toHtml author

          input_
            [ class_ "mr-2 px-3 py-2 border rounded-md text-sm",
              type_ "date",
              name_ "startDate",
              placeholder_ "Start date",
              value_ (maybe "" display startDateField)
            ]

          span_ [class_ "mr-2 text-sm text-gray-500"] "to"

          input_
            [ class_ "mr-2 px-3 py-2 border rounded-md text-sm",
              type_ "date",
              name_ "endDate",
              placeholder_ "End date",
              value_ (maybe "" display endDateField)
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
          th_ [class_ "px-4 py-3"] "Published"

      -- Table Body
      tbody_ [id_ "posts-table"] $ do
        forM_ posts $ \(BlogPosts.Domain {..}, user) -> do
          tr_ [class_ "border-t hover:bg-gray-50"] $ do
            td_ [class_ "px-4 py-3"] $ input_ [type_ "checkbox", name_ "ids", class_ "row-checkbox", value_ (display dId)]
            td_ [class_ "px-4 py-3 font-medium text-gray-900"] $ do
              button_ [hxGet_ [i|/#{blogEditUrl dId}|], hxTarget_ "#admin-main", hxPushUrl_ "true"] $
                Lucid.toHtml $
                  display dTitle
            td_ [class_ "px-4 py-3"] $ Lucid.toHtml $ display $ fuDisplayName user
            td_ [class_ "px-4 py-3 text-xs"] $ do
              maybe "unpublished" (const "published") dPublishedAt
              br_ []
              Lucid.toHtml $ maybe "" (display . utctDay) dPublishedAt
            td_ [class_ "px-4 py-3"] $
              div_ [class_ "px-5 font-medium inline-flex"] $
                label_ [class_ "inline-flex items-center cursor-pointer"] $ do
                  input_
                    ( [ hxPatch_ [i|/#{blogTogglePublishUrl dId}|],
                        hxTrigger_ "change",
                        hxFollowDirects_,
                        hxSelect_ "#posts-table",
                        hxTarget_ "#posts-table",
                        hxSwap_ "outerHTML",
                        type_ "checkbox",
                        name_ "published",
                        class_ "sr-only peer",
                        value_ "true"
                      ]
                        <> catMaybes [if isJust dPublishedAt then Just checked_ else Nothing]
                    )
                  div_
                    [ class_ "relative w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-green-300 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:w-5 after:h-5 after:transition-all peer-checked:bg-green-600"
                    ]
                    mempty

blogNewUrl :: Link.URI
blogNewUrl = Link.linkURI blogNewGetLink

blogEditUrl :: BlogPosts.Id -> Link.URI
blogEditUrl = Link.linkURI . flip blogIdEditGetLink Nothing

adminBlogGetUrl :: Link.URI
adminBlogGetUrl = Link.linkURI adminBlogGetLink

blogTogglePublishUrl :: BlogPosts.Id -> Link.URI
blogTogglePublishUrl = Link.linkURI . blogTogglePublish

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
  Maybe (OptionalField StartDate) ->
  Maybe (OptionalField EndDate) ->
  Maybe (OptionalField DisplayName) ->
  m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
handler (Auth.Authz user@FullUser {..} _) hxTrigger searchQuery (flatten -> startDate) (flatten -> endDate) (flatten -> author) = do
  Observability.handlerSpan "GET /admin/blog" () (display . Servant.getResponse) $
    if fuIsAdmin
      then do
        postsWithAuthors <- do
          allPosts <- fmap (\(x, y, z) -> (BlogPosts.toDomain (x, y), z)) <$> execQuerySpanThrow BlogPosts.getBlogPostsWithUser

          pure $
            withQueryParam filterByStartDate startDate $
              withQueryParam filterByEndDate endDate $
                withQueryParam filterPosts searchQuery $
                  withQueryParam filterByAuthor author allPosts

        let searchForm = SearchForm searchQuery author startDate endDate Nothing
            page = template postsWithAuthors searchForm
        case hxTrigger of
          Just True ->
            pure $ Servant.addHeader "HX-Request" page
          _ -> do
            pageWithFrame <- loadFrameWithNavAdmin (Auth.IsLoggedIn user) "about-tab" page
            pure $ Servant.addHeader "HX-Request" pageWithFrame
      else renderUnauthorized $ Auth.IsLoggedIn user

withQueryParam :: (queryParam -> xs -> xs) -> Maybe queryParam -> xs -> xs
withQueryParam = maybe id

renderUnauthorized :: (MonadIO m, Log.MonadLog m, MonadThrow m) => Auth.LoggedIn -> m (Servant.Headers '[Servant.Header "Vary" Text] (Lucid.Html ()))
renderUnauthorized loginState = do
  page <- loadFrameWithNav loginState "about-tab" unauthorized
  pure $ Servant.addHeader "HX-Request" page

filterByAuthor :: DisplayName -> [(BlogPosts.Domain, FullUser)] -> [(BlogPosts.Domain, FullUser)]
filterByAuthor name = filter (((== name) . fuDisplayName) . snd)

filterPosts :: SearchQuery -> [(BlogPosts.Domain, FullUser)] -> [(BlogPosts.Domain, FullUser)]
filterPosts (SearchQuery q) xs =
  Fuzzy.original <$> Fuzzy.filter q xs "" "" (\(p, _) -> mconcat [BlogPosts.getSubject (BlogPosts.dTitle p), BlogPosts.getBody (BlogPosts.dContent p)]) False

filterByStartDate :: StartDate -> [(BlogPosts.Domain, FullUser)] -> [(BlogPosts.Domain, FullUser)]
filterByStartDate (StartDate start) =
  filter $ \(BlogPosts.Domain {..}, _) -> maybe False ((>= start) . utctDay) dPublishedAt

filterByEndDate :: EndDate -> [(BlogPosts.Domain, FullUser)] -> [(BlogPosts.Domain, FullUser)]
filterByEndDate (EndDate end) =
  filter $ \(BlogPosts.Domain {..}, _) -> maybe False ((< end) . utctDay) dPublishedAt
