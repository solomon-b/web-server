module API.Dashboard.Get
  ( Route,
    handler,
  )
where

import App.Auth (CookieAuth)
import Auth.Roles (RoleAuth (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Auth.Roles (SomeRole (..))
import Text.HTML (HTML)
import View (dashboardPage)

--------------------------------------------------------------------------------

type Route =
  CookieAuth
    :> "dashboard"
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

-- | A plain authenticated route on the shared cookie-auth tag. It receives the
-- existential 'SomeRole' (the role is not needed here) and unpacks the user.
handler ::
  (Applicative m) =>
  SomeRole RoleAuth ->
  m (Lucid.Html ())
handler (SomeRole _ (RoleAuth user _)) =
  pure $ dashboardPage user
