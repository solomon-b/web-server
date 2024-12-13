{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

{-
TODO:
- Basic HTMX Widgets
  - Login
  - Register
  - Navbar
  - Mailing List Signup
- HTML/HTMX Starter Template
-}

--------------------------------------------------------------------------------

import API
import App.Auth (authHandler)
import App.Config
import App.Context
import App.Errors.HTML (error401template, error403template, error404template, error500template)
import App.Monad
import Control.Error (isLeft)
import Control.Exception (catch)
import Control.Monad (void, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.CaseInsensitive qualified as CI
import Data.Data (Proxy (..))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Effects.Database.Class (execStatement, healthCheck)
import Effects.Observability qualified as Observability
import Hasql.Connection qualified as HSQL
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Pool.Config as HSQL.Pool.Config
import Log (runLogT)
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace qualified as OTEL
import Servant (Context ((:.)))
import Servant qualified
import System.Posix.Signals qualified as Posix

--------------------------------------------------------------------------------

runApp :: customCtx -> IO ()
runApp ctx =
  Log.withJsonStdOutLogger $ \stdOutLogger -> do
    getConfig >>= \case
      Nothing ->
        Log.runLogT "webserver-backend" stdOutLogger Log.defaultLogLevel $ Log.logAttention "Config Failure" (show ())
      Just AppConfig {..} -> do
        let PostgresConfig {..} = appConfigPostgresSettings
        -- TODO: Is it weird to be instantiating 'LogT' multiple times?
        Log.runLogT "webserver-backend" stdOutLogger Log.defaultLogLevel $
          Log.logInfo "Launching Service" (KeyMap.fromList ["port" .= warpConfigPort appConfigWarpSettings, "environment" .= appConfigEnvironment])

        -- Setup DB Pool
        let hostname = if isProduction appConfigEnvironment then appConfigHostname else Hostname $ "localhost:" <> display (warpConfigPort appConfigWarpSettings)
        let hsqlSettings = HSQL.settings (fold postgresConfigHost) (fromMaybe 0 postgresConfigPort) (fold postgresConfigUser) (fold postgresConfigPassword) (fold postgresConfigDB)
        let poolSettings = HSQL.Pool.Config.settings [HSQL.Pool.Config.staticConnectionSettings hsqlSettings]
        pgPool <- HSQL.Pool.acquire poolSettings

        -- TODO: We need better logging:
        healthCheckResult <- flip runReaderT pgPool $ execStatement healthCheck
        when (isLeft healthCheckResult) $ error $ "Postgres healthcheck failed: " <> show healthCheckResult

        let cfg = customFormatters :. authHandler pgPool :. Servant.EmptyContext

        -- Run App with tracing
        Observability.withTracer appConfigObservability $ \tracerProvider mkTracer -> do
          let tracer = mkTracer OTEL.tracerOptions
          let otelMiddleware = newOpenTelemetryWaiMiddleware' tracerProvider
          Warp.runSettings (warpSettings stdOutLogger appConfigWarpSettings) (otelMiddleware $ mkApp cfg (AppContext stdOutLogger pgPool tracer appConfigSmtp hostname appConfigEnvironment ctx))

notFoundFormatter :: Servant.NotFoundErrorFormatter
notFoundFormatter _ =
  Servant.err404 {Servant.errBody = error404template}

customFormatters :: Servant.ErrorFormatters
customFormatters =
  Servant.defaultErrorFormatters {Servant.notFoundErrorFormatter = notFoundFormatter}

warpSettings :: Log.Logger -> WarpConfig -> Warp.Settings
warpSettings logger' WarpConfig {..} =
  Warp.defaultSettings
    & Warp.setServerName warpConfigServerName
    & Warp.setLogger (warpStructuredLogger logger')
    & Warp.setPort warpConfigPort
    & Warp.setGracefulShutdownTimeout (Just warpConfigTimeout)
    & Warp.setInstallShutdownHandler shutdownHandler

warpStructuredLogger :: Log.Logger -> Wai.Request -> Status.Status -> Maybe Integer -> IO ()
warpStructuredLogger logger' req s sz = do
  reqBody <- Wai.getRequestBodyChunk req
  Log.runLogT "webserver-backend" logger' Log.defaultLogLevel $
    Log.logInfo "Request" $
      KeyMap.fromList $
        [ "statusCode" .= Status.statusCode s,
          "method" .= Text.Encoding.decodeUtf8 (Wai.requestMethod req),
          "httpVersion" .= show (Wai.httpVersion req),
          "path" .= Text.Encoding.decodeUtf8 (Wai.rawPathInfo req),
          "queryString" .= fmap (bimap Text.Encoding.decodeUtf8 (fmap Text.Encoding.decodeUtf8)) (Wai.queryString req),
          "headers" .= fmap (bimap (Text.Encoding.decodeUtf8 . CI.foldedCase) Text.Encoding.decodeUtf8) (Wai.requestHeaders req),
          "isSecure" .= Wai.isSecure req,
          "remoteHost" .= show (Wai.remoteHost req),
          "requestBody" .= Text.Encoding.decodeUtf8 reqBody
        ]
          <> catMaybes [fmap ("requestSize" .=) sz]

shutdownHandler :: IO () -> IO ()
shutdownHandler closeSocket =
  void $ Posix.installHandler Posix.sigTERM (Posix.CatchOnce closeSocket) Nothing

--------------------------------------------------------------------------------

interpret :: AppContext ctx -> AppM ctx x -> Servant.Handler x
interpret ctx@AppContext {appLogger} (AppM appM) =
  Servant.Handler $
    ExceptT $
      catch (Right <$> appM ctx (Log.LoggerEnv appLogger "webserver-backend" [] [] Log.defaultLogLevel)) $
        \e ->
          case Servant.errHTTPCode e of
            401 -> pure $ Left Servant.err401 {Servant.errBody = error401template}
            403 -> pure $ Left Servant.err401 {Servant.errBody = error403template}
            404 -> pure $ Left Servant.err401 {Servant.errBody = error404template}
            500 -> pure $ Left Servant.err401 {Servant.errBody = error500template}
            _ -> pure $ Left e

mkApp :: Servant.Context ServantContext -> AppContext ctx -> Servant.Application
mkApp cfg ctx =
  Servant.serveWithContext (Proxy @API) cfg $
    Servant.hoistServerWithContext
      (Proxy @API)
      (Proxy @ServantContext)
      (interpret ctx)
      (server $ appEnvironment ctx)
