module API.Home.Get
  ( Route,
    handler,
  )
where

import App.Auth qualified as Auth
import App.Config (Environment)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Hasql.Pool qualified as HSQL
import Log qualified
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import View (homePage)

--------------------------------------------------------------------------------

type Route =
  Servant.Header "Cookie" Text
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

handler ::
  ( MonadIO m,
    MonadReader env m,
    Has HSQL.Pool env,
    Has Environment env,
    Log.MonadLog m
  ) =>
  Maybe Text ->
  m (Lucid.Html ())
handler cookie = do
  env <- Reader.asks Has.getter
  loginState <- Auth.userLoginState env cookie
  pure $ homePage loginState
