module API.Admin.Get
  ( Route,
    handler,
  )
where

--------------------------------------------------------------------------------

import App.Auth (CookieAuthTag)
import Auth.Roles (Role (..), RoleAuth (..))
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (RequireRole, Satisfies (..))
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Gated on @'Editor@: satisfied by @Editor@ and @Admin@ (hierarchical), but
-- not @Viewer@. Anonymous requests hit the auth handler's 401/redirect; an
-- authenticated @Viewer@ gets a non-fatal 403.
type Route =
  RequireRole CookieAuthTag 'Editor
    :> "admin"
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

handler ::
  (Applicative m) =>
  Satisfies 'Editor RoleAuth ->
  m (Lucid.Html ())
handler (Satisfies _ (RoleAuth role user _)) =
  pure $ adminPage role user

adminPage :: Role -> User.Model -> Lucid.Html ()
adminPage role user =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ Lucid.title_ "Admin"
    Lucid.body_ $ do
      Lucid.h1_ "Admin Panel"
      Lucid.p_ $ do
        "Signed in as "
        Lucid.toHtml (display (User.mEmail user))
      Lucid.p_ $ do
        "Your role: "
        Lucid.toHtml (show role)
      Lucid.p_ "This page requires the Editor role (or higher)."
