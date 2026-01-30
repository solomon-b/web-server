module API.Dashboard.Get
  ( Route,
    handler,
  )
where

import App.Auth (Authz (..))
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import View (dashboardPage)

--------------------------------------------------------------------------------

type Route =
  Servant.AuthProtect "cookie-auth"
    :> "dashboard"
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

handler ::
  (Applicative m) =>
  Authz ->
  m (Lucid.Html ())
handler Authz {authzUser} =
  pure $ dashboardPage authzUser
