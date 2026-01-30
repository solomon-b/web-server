module API.User.Register.Get
  ( Route,
    handler,
  )
where

import Domain.Types.EmailAddress (EmailAddress)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import View (registerPage)

--------------------------------------------------------------------------------

type Route =
  "user"
    :> "register"
    :> Servant.QueryParam "email" EmailAddress
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

handler ::
  (Applicative m) =>
  Maybe EmailAddress ->
  m (Lucid.Html ())
handler emailQueryParam =
  pure $ registerPage emailQueryParam
