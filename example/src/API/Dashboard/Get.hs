module API.Dashboard.Get
  ( ViewerRoute,
    viewerHandler,
    EditorRoute,
    editorHandler,
    AdminRoute,
    adminHandler,
  )
where

import App.Auth (CookieAuthTag)
import Auth.Roles (Role (..), RoleAuth (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole, Satisfies (..))
import Text.HTML (HTML)
import View (adminDashboardPage, editorDashboardPage, viewerDashboardPage)

--------------------------------------------------------------------------------
-- One dashboard per role, all on the SAME path (@\/dashboard@). Each is a
-- 'RequireRole' alternative; a role mismatch is a non-fatal 403, so Servant
-- falls through to the next alternative until a gate passes. They are wired
-- most-restrictive-first (Admin, Editor, Viewer) so the hierarchical @>=@
-- relation doesn't let a looser gate shadow a tighter one. One URL, each role
-- served its own view.

type ViewerRoute =
  RequireRole CookieAuthTag 'Viewer
    :> "dashboard"
    :> Servant.Get '[HTML] (Lucid.Html ())

type EditorRoute =
  RequireRole CookieAuthTag 'Editor
    :> "dashboard"
    :> Servant.Get '[HTML] (Lucid.Html ())

type AdminRoute =
  RequireRole CookieAuthTag 'Admin
    :> "dashboard"
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

viewerHandler :: (Applicative m) => Satisfies 'Viewer RoleAuth -> m (Lucid.Html ())
viewerHandler (Satisfies _ (RoleAuth user _)) = pure $ viewerDashboardPage user

editorHandler :: (Applicative m) => Satisfies 'Editor RoleAuth -> m (Lucid.Html ())
editorHandler (Satisfies _ (RoleAuth user _)) = pure $ editorDashboardPage user

adminHandler :: (Applicative m) => Satisfies 'Admin RoleAuth -> m (Lucid.Html ())
adminHandler (Satisfies _ (RoleAuth user _)) = pure $ adminDashboardPage user
