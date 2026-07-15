{-# OPTIONS_GHC -Wno-orphans #-}

module App.Auth.RoleSpec (spec) where

import App.Auth.Role (CheckRole (..), HasRole (..), Proxy (..), RequireRole)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class qualified
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Network.HTTP.Types (hAccept)
import Network.Wai (Application, Request, requestHeaders)
import Servant.API (Get, JSON, type (:<|>) (..), type (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server (Context (..), Handler, Server, err401, serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Hspec.Wai (get, matchStatus, request, shouldRespondWith, with)
import Test.Hspec.Wai.Internal (runWaiSession)

--------------------------------------------------------------------------------
-- Mock role type

data TestRole = Viewer | Editor | Admin
  deriving (Eq, Ord, Show)

instance CheckRole 'Viewer where
  type RoleType 'Viewer = TestRole
  checkRole _ role = role >= Viewer

instance CheckRole 'Editor where
  type RoleType 'Editor = TestRole
  checkRole _ role = role >= Editor

instance CheckRole 'Admin where
  type RoleType 'Admin = TestRole
  checkRole _ role = role >= Admin

--------------------------------------------------------------------------------
-- Mock auth type

newtype TestAuth = TestAuth { testAuthRole :: TestRole }
  deriving (Show)

instance HasRole TestAuth TestRole where
  getRole = testAuthRole

--------------------------------------------------------------------------------
-- AuthServerData type instance

type instance AuthServerData (AuthProtect "test-auth") = TestAuth

--------------------------------------------------------------------------------
-- Mock auth handler

-- | Reads the "X-Test-Role" header to determine the user's role.
-- Missing header -> 401. Values: "viewer", "editor", "admin".
testAuthHandler :: AuthHandler Request TestAuth
testAuthHandler = mkAuthHandler $ \req ->
  case lookup "X-Test-Role" (requestHeaders req) of
    Nothing -> throwError err401
    Just "viewer" -> pure (TestAuth Viewer)
    Just "editor" -> pure (TestAuth Editor)
    Just "admin" -> pure (TestAuth Admin)
    Just _ -> throwError err401

--------------------------------------------------------------------------------
-- Test API (separate paths)

type AdminAPI =
  RequireRole "test-auth" 'Admin
    :> "admin"
    :> Get '[JSON] String

type EditorAPI =
  RequireRole "test-auth" 'Editor
    :> "editor"
    :> Get '[JSON] String

type ViewerAPI =
  RequireRole "test-auth" 'Viewer
    :> "viewer"
    :> Get '[JSON] String

type TestAPI = AdminAPI :<|> EditorAPI :<|> ViewerAPI

testServer :: Server TestAPI
testServer = adminHandler :<|> editorHandler :<|> viewerHandler
  where
    adminHandler :: TestAuth -> Handler String
    adminHandler auth = pure $ "admin: " <> show (testAuthRole auth)

    editorHandler :: TestAuth -> Handler String
    editorHandler auth = pure $ "editor: " <> show (testAuthRole auth)

    viewerHandler :: TestAuth -> Handler String
    viewerHandler auth = pure $ "viewer: " <> show (testAuthRole auth)

testApp :: IO Application
testApp =
  pure $
    serveWithContext
      (Proxy @TestAPI)
      (testAuthHandler :. EmptyContext)
      testServer

--------------------------------------------------------------------------------
-- Test API (same path, role-based fallthrough)
--
-- Routes ordered most-restrictive-first:
--   Admin  -> "admin panel"
--   Editor -> "editor panel"
--   Viewer -> "viewer panel"

type PanelAdminAPI =
  RequireRole "test-auth" 'Admin
    :> "panel"
    :> Get '[JSON] String

type PanelEditorAPI =
  RequireRole "test-auth" 'Editor
    :> "panel"
    :> Get '[JSON] String

type PanelViewerAPI =
  RequireRole "test-auth" 'Viewer
    :> "panel"
    :> Get '[JSON] String

type FallthroughAPI = PanelAdminAPI :<|> PanelEditorAPI :<|> PanelViewerAPI

fallthroughServer :: Server FallthroughAPI
fallthroughServer = panelAdmin :<|> panelEditor :<|> panelViewer
  where
    panelAdmin :: TestAuth -> Handler String
    panelAdmin _ = pure "admin panel"

    panelEditor :: TestAuth -> Handler String
    panelEditor _ = pure "editor panel"

    panelViewer :: TestAuth -> Handler String
    panelViewer _ = pure "viewer panel"

fallthroughApp :: IO Application
fallthroughApp =
  pure $
    serveWithContext
      (Proxy @FallthroughAPI)
      (testAuthHandler :. EmptyContext)
      fallthroughServer

--------------------------------------------------------------------------------
-- Test API (auth invocation counting)

-- | Auth handler that increments an IORef each time it is called.
countingAuthHandler :: IORef Int -> AuthHandler Request TestAuth
countingAuthHandler counter = mkAuthHandler $ \req -> do
  Control.Monad.IO.Class.liftIO $ modifyIORef' counter (+ 1)
  case lookup "X-Test-Role" (requestHeaders req) of
    Nothing -> throwError err401
    Just "viewer" -> pure (TestAuth Viewer)
    Just "editor" -> pure (TestAuth Editor)
    Just "admin" -> pure (TestAuth Admin)
    Just _ -> throwError err401

countingApp :: IORef Int -> IO Application
countingApp counter =
  pure $
    serveWithContext
      (Proxy @FallthroughAPI)
      (countingAuthHandler counter :. EmptyContext)
      fallthroughServer

--------------------------------------------------------------------------------
-- Test API (public route behind role gates)
--
-- Regression guard: a trailing unauthenticated route is UNREACHABLE when it
-- sits behind RequireRole alternatives on the same path. Because authentication
-- failure is fatal, an unauthenticated request short-circuits at the first
-- RequireRole (401) and never falls through to the public route. That route is
-- dead code here: only an authenticated user whose role is below every gate
-- could reach it, and no such role exists in TestRole.

type PanelAnonAPI =
  "panel"
    :> Get '[JSON] String

type AnonFallthroughAPI =
  PanelAdminAPI :<|> PanelEditorAPI :<|> PanelViewerAPI :<|> PanelAnonAPI

anonFallthroughServer :: Server AnonFallthroughAPI
anonFallthroughServer = panelAdmin :<|> panelEditor :<|> panelViewer :<|> panelAnon
  where
    panelAdmin :: TestAuth -> Handler String
    panelAdmin _ = pure "admin panel"

    panelEditor :: TestAuth -> Handler String
    panelEditor _ = pure "editor panel"

    panelViewer :: TestAuth -> Handler String
    panelViewer _ = pure "viewer panel"

    panelAnon :: Handler String
    panelAnon = pure "anon panel"

anonFallthroughApp :: IO Application
anonFallthroughApp =
  pure $
    serveWithContext
      (Proxy @AnonFallthroughAPI)
      (testAuthHandler :. EmptyContext)
      anonFallthroughServer

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "RequireRole" $ do
    -- Role x Route matrix (3 roles x 3 routes + unauthenticated = 12 cases)
    --
    --              | /viewer (Viewer) | /editor (Editor) | /admin (Admin)
    -- -------------|------------------|------------------|---------------
    -- Viewer       |       200        |       403        |      403
    -- Editor       |       200        |       200        |      403
    -- Admin        |       200        |       200        |      200
    -- No auth      |       401        |       401        |      401

    with testApp $ do
      describe "/viewer route (requires Viewer)" $ do
        it "Viewer  -> 200" $ do
          request "GET" "/viewer" [("X-Test-Role", "viewer"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        it "Editor  -> 200" $ do
          request "GET" "/viewer" [("X-Test-Role", "editor"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        it "Admin   -> 200" $ do
          request "GET" "/viewer" [("X-Test-Role", "admin"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        it "No auth -> 401" $ do
          get "/viewer" `shouldRespondWith` 401

      describe "/editor route (requires Editor)" $ do
        it "Viewer  -> 403" $ do
          request "GET" "/editor" [("X-Test-Role", "viewer"), (hAccept, "application/json")] "" `shouldRespondWith` 403
        it "Editor  -> 200" $ do
          request "GET" "/editor" [("X-Test-Role", "editor"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        it "Admin   -> 200" $ do
          request "GET" "/editor" [("X-Test-Role", "admin"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        it "No auth -> 401" $ do
          get "/editor" `shouldRespondWith` 401

      describe "/admin route (requires Admin)" $ do
        it "Viewer  -> 403" $ do
          request "GET" "/admin" [("X-Test-Role", "viewer"), (hAccept, "application/json")] "" `shouldRespondWith` 403
        it "Editor  -> 403" $ do
          request "GET" "/admin" [("X-Test-Role", "editor"), (hAccept, "application/json")] "" `shouldRespondWith` 403
        it "Admin   -> 200" $ do
          request "GET" "/admin" [("X-Test-Role", "admin"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        it "No auth -> 401" $ do
          get "/admin" `shouldRespondWith` 401

    with fallthroughApp $ do
      describe "same path, role-based fallthrough" $ do
        it "Admin gets admin panel (first match)" $ do
          request "GET" "/panel" [("X-Test-Role", "admin"), (hAccept, "application/json")] ""
            `shouldRespondWith` "\"admin panel\"" {matchStatus = 200}

        it "Editor falls through Admin, matches Editor" $ do
          request "GET" "/panel" [("X-Test-Role", "editor"), (hAccept, "application/json")] ""
            `shouldRespondWith` "\"editor panel\"" {matchStatus = 200}

        it "Viewer falls through Admin and Editor, matches Viewer" $ do
          request "GET" "/panel" [("X-Test-Role", "viewer"), (hAccept, "application/json")] ""
            `shouldRespondWith` "\"viewer panel\"" {matchStatus = 200}

        it "unauthenticated request returns 401 (auth failure is fatal)" $ do
          get "/panel" `shouldRespondWith` 401

    with anonFallthroughApp $ do
      describe "public route behind RequireRole is unreachable (auth failure is fatal)" $ do
        it "unauthenticated -> 401, not the public route" $ do
          get "/panel" `shouldRespondWith` 401
        it "authenticated viewer still matches the viewer gate (public route is dead code)" $ do
          request "GET" "/panel" [("X-Test-Role", "viewer"), (hAccept, "application/json")] ""
            `shouldRespondWith` "\"viewer panel\"" {matchStatus = 200}

    describe "auth handler invocation count" $ do
      it "Admin matches first alternative: auth runs 1 time" $ do
        counter <- newIORef 0
        app <- countingApp counter
        flip runWaiSession app $
          request "GET" "/panel" [("X-Test-Role", "admin"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        readIORef counter `shouldReturn` 1

      it "Editor falls through 1 alternative: auth runs 2 times" $ do
        counter <- newIORef 0
        app <- countingApp counter
        flip runWaiSession app $
          request "GET" "/panel" [("X-Test-Role", "editor"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        readIORef counter `shouldReturn` 2

      it "Viewer falls through 2 alternatives: auth runs 3 times" $ do
        counter <- newIORef 0
        app <- countingApp counter
        flip runWaiSession app $
          request "GET" "/panel" [("X-Test-Role", "viewer"), (hAccept, "application/json")] "" `shouldRespondWith` 200
        readIORef counter `shouldReturn` 3
