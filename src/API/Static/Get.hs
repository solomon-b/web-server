module API.Static.Get where

--------------------------------------------------------------------------------

import Config (Environment (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

type Route = "static" :> Servant.Raw

handler :: (MonadReader env m, Has Environment env) => Environment -> Servant.ServerT Servant.Raw m
handler = \case
  Production -> Servant.serveDirectoryWebApp "/static"
  Development -> Servant.serveDirectoryWebApp "./static"
