{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import App.Auth (authHandler, execStatement, healthCheck)
import App.Config
import App.Context
import App.Monad
import Control.Error (isLeft)
import Control.Exception (catch)
import Control.Monad (void, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.CaseInsensitive qualified as CI
import Data.Data (Proxy (..))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import App.Observability qualified as Observability
import Hasql.Connection qualified as HSQL
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Pool.Config as HSQL.Pool.Config
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

runApp ::
  forall api ctx.
  Servant.HasServer api ServantContext =>
  (Environment -> Servant.ServerT api (AppM ctx)) ->
  ctx ->
  IO ()
runApp server ctx = do
  time <- getCurrentTime
  Log.withJsonStdOutLogger $ \stdOutLogger -> do
    getConfig >>= \case
      Nothing -> do
        let logEnv = Log.LoggerEnv stdOutLogger "webserver-backend" [] [] Log.LogInfo
        Log.logMessageIO logEnv time Log.LogAttention "Config Failure" (Aeson.toJSON ())
      Just AppConfig {..} -> do
        let hostname = if isProduction appConfigEnvironment then appConfigHostname else Hostname $ "localhost:" <> display (warpConfigPort appConfigWarpSettings)

        -- Log Env
        let maxLogLevel = mkLogLevel $ observabilityConfigVerbosity appConfigObservability
            logEnv = Log.LoggerEnv stdOutLogger "webserver-backend" [] [] maxLogLevel
        Log.logMessageIO logEnv time Log.LogInfo "Launching Service" (Aeson.toJSON $ KeyMap.fromList ["port" .= warpConfigPort appConfigWarpSettings, "environment" .= appConfigEnvironment, "log-level" .= maxLogLevel])

        -- DB Pool
        let PostgresConfig {..} = appConfigPostgresSettings
        let hsqlSettings = HSQL.settings (fold postgresConfigHost) (fromMaybe 0 postgresConfigPort) (fold postgresConfigUser) (fold postgresConfigPassword) (fold postgresConfigDB)
        let poolSettings = HSQL.Pool.Config.settings [HSQL.Pool.Config.staticConnectionSettings hsqlSettings]
        pgPool <- HSQL.Pool.acquire poolSettings

        -- DB Healthcheck
        healthCheckResult <- flip runReaderT pgPool $ execStatement healthCheck
        when (isLeft healthCheckResult) $
          Log.logMessageIO logEnv time Log.LogAttention "webserver-backend: Postres healthcheck failed" (Aeson.toJSON $ KeyMap.fromList ["error" .= show healthCheckResult])

        -- Run App with tracing
        Observability.withTracer appConfigObservability $ \tracerProvider mkTracer -> do
          let tracer = mkTracer OTEL.tracerOptions
          let otelMiddleware = newOpenTelemetryWaiMiddleware' tracerProvider

          let servantContext = authHandler pgPool :. Servant.EmptyContext
              appContext = AppContext pgPool tracer hostname appConfigEnvironment logEnv ctx
              warpSettings = mkWarpSettings logEnv appConfigWarpSettings
          Warp.runSettings warpSettings (otelMiddleware $ mkApp @api server servantContext appContext)

mkLogLevel :: Verbosity -> Log.LogLevel
mkLogLevel = \case
  Quiet -> Log.LogAttention
  Brief -> Log.LogInfo
  Verbose -> Log.LogInfo
  Debug -> Log.LogTrace

mkWarpSettings :: Log.LoggerEnv -> WarpConfig -> Warp.Settings
mkWarpSettings logEnv WarpConfig {..} =
  Warp.defaultSettings
    & Warp.setServerName warpConfigServerName
    & Warp.setLogger (mkWarpLogger logEnv)
    & Warp.setPort warpConfigPort
    & Warp.setGracefulShutdownTimeout (Just warpConfigTimeout)
    & Warp.setInstallShutdownHandler shutdownHandler

mkWarpLogger :: Log.LoggerEnv -> Wai.Request -> Status.Status -> Maybe Integer -> IO ()
mkWarpLogger logEnv req s sz = do
  time <- getCurrentTime
  reqBody <- Wai.getRequestBodyChunk req
  Log.logMessageIO logEnv time Log.LogInfo "Request" $
    Aeson.toJSON $
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
interpret ctx (AppM appM) =
  Servant.Handler $
    ExceptT $
      catch (Right <$> appM ctx) $
        \e -> pure $ Left e

mkApp ::
  forall api ctx.
  Servant.HasServer api ServantContext =>
  (Environment -> Servant.ServerT api (AppM ctx)) ->
  Servant.Context ServantContext ->
  AppContext ctx ->
  Servant.Application
mkApp server cfg ctx =
  Servant.serveWithContext (Proxy @api) cfg $
    Servant.hoistServerWithContext
      (Proxy @api)
      (Proxy @ServantContext)
      (interpret ctx)
      (server $ appEnvironment ctx)
