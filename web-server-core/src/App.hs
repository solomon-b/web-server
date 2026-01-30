{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App
  ( -- * Application Entry Points
    runApp,
    withAppResources,
    runServer,

    -- * Server Configuration
    ServerContext,
    mkApp,
    interpret,

    -- * Utility Functions
    acquirePool,
    mkLogLevel,
    mkWarpSettings,
  )
where

--------------------------------------------------------------------------------

import App.Auth (Authz, authHandler, execStatement, healthCheck)
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
import Data.Function ((&))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import Hasql.Connection.Setting qualified as HSQL.Setting
import Hasql.Connection.Setting.Connection qualified as HSQL.Connection
import Hasql.Connection.Setting.Connection.Param qualified as HSQL.Params
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Pool.Config as HSQL.Pool.Config
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Network.HTTP.Types.Status qualified as Status
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context ((:.)))
import Servant qualified
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Posix.Signals qualified as Posix

--------------------------------------------------------------------------------

-- | Acquire application resources in a bracket pattern.
-- This allows running code with access to the initialized AppContext
-- without starting the HTTP server - useful for background jobs,
-- testing, or custom resource coordination.
withAppResources :: ctx -> (AppContext ctx -> IO a) -> IO a
withAppResources ctx action = do
  time <- getCurrentTime
  Log.withJsonStdOutLogger $ \stdOutLogger -> do
    getConfig >>= \case
      Nothing -> do
        let logEnv = Log.LoggerEnv stdOutLogger "webserver-backend" [] [] Log.LogInfo
        Log.logMessageIO logEnv time Log.LogAttention "Config Failure" (Aeson.toJSON ())
        error "Failed to load configuration"
      Just AppConfig {..} -> do
        let hostname = if isProduction appConfigEnvironment then appConfigHostname else Hostname $ "localhost:" <> display (warpConfigPort appConfigWarpSettings)

        -- Log Env
        let maxLogLevel = mkLogLevel $ verbosityConfigVerbosity appConfigVerbosity
            logEnv = Log.LoggerEnv stdOutLogger "webserver-backend" [] [] maxLogLevel
        Log.logMessageIO logEnv time Log.LogInfo "Launching Service" (Aeson.toJSON $ KeyMap.fromList ["port" .= warpConfigPort appConfigWarpSettings, "environment" .= appConfigEnvironment, "log-level" .= maxLogLevel])

        Log.logMessageIO logEnv time Log.LogInfo "Acquiring Postgres Connection Pool" (Aeson.object [])
        pgPool <- acquirePool appConfigPostgresSettings

        -- DB Healthcheck
        healthCheckResult <- flip runReaderT pgPool $ execStatement healthCheck
        when (isLeft healthCheckResult) $
          Log.logMessageIO logEnv time Log.LogAttention "webserver-backend: Postres healthcheck failed" (Aeson.toJSON $ KeyMap.fromList ["error" .= show healthCheckResult])

        let appContext =
              AppContext
                { appDbPool = pgPool,
                  appHostname = hostname,
                  appEnvironment = appConfigEnvironment,
                  appLoggerEnv = logEnv,
                  appWarpConfig = appConfigWarpSettings,
                  appCustom = ctx
                }
        action appContext

-- | Run the HTTP server with an already-initialized AppContext.
-- Useful when you need to coordinate the server with other subsystems
-- or run it alongside background jobs.
runServer ::
  forall api ctx.
  (Servant.HasServer api ServerContext) =>
  (Environment -> Servant.ServerT api (AppM ctx)) ->
  AppContext ctx ->
  IO ()
runServer server appCtx = do
  let servantContext = authHandler (appDbPool appCtx) :. Servant.EmptyContext
      warpSettings = mkWarpSettings (appLoggerEnv appCtx) (appWarpConfig appCtx)
  Warp.runSettings warpSettings (mkApp @api server servantContext appCtx)

-- | Run the application server. This is the main entry point that combines
-- resource acquisition with server execution.
runApp ::
  forall api ctx.
  (Servant.HasServer api ServerContext) =>
  (Environment -> Servant.ServerT api (AppM ctx)) ->
  ctx ->
  IO ()
runApp server ctx = withAppResources ctx (runServer @api server)

acquirePool :: PostgresConfig -> IO HSQL.Pool.Pool
acquirePool PostgresConfig {..} = do
  let hsqlSettings :: Maybe [HSQL.Setting.Setting]
      hsqlSettings = do
        host <- HSQL.Params.host . TE.decodeUtf8 <$> postgresConfigHost
        port <- HSQL.Params.port <$> postgresConfigPort
        user <- HSQL.Params.user . TE.decodeUtf8 <$> postgresConfigUser
        password <- HSQL.Params.password . TE.decodeUtf8 <$> postgresConfigPassword
        db <- HSQL.Params.dbname . TE.decodeUtf8 <$> postgresConfigDB
        pure [HSQL.Setting.connection $ HSQL.Connection.params [host, port, user, password, db]]
  let poolSettings = HSQL.Pool.Config.settings $ pure $ staticConnectionSettings $ fromMaybe [] hsqlSettings
  HSQL.Pool.acquire poolSettings

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
    & Warp.setHost "0.0.0.0"
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

type ServerContext = '[AuthHandler Request Authz]

interpret :: AppContext ctx -> AppM ctx x -> Servant.Handler x
interpret ctx (AppM appM) =
  Servant.Handler $
    ExceptT $
      catch (Right <$> appM ctx) $
        \e -> pure $ Left e

mkApp ::
  forall api ctx.
  (Servant.HasServer api ServerContext) =>
  (Environment -> Servant.ServerT api (AppM ctx)) ->
  Context ServerContext ->
  AppContext ctx ->
  Servant.Application
mkApp server fullCtx appCtx =
  Servant.serveWithContext (Proxy @api) fullCtx $
    Servant.hoistServerWithContext
      (Proxy @api)
      (Proxy @ServerContext)
      (interpret appCtx)
      (server $ appEnvironment appCtx)
