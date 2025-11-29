module App.Context where

--------------------------------------------------------------------------------

import App.Auth (Authz)
import App.Config
import Data.Has qualified as Has
import Hasql.Pool qualified as HSQL (Pool)
import Log qualified
import Network.Wai qualified as Wai
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Servant.Server.Experimental.Auth (AuthHandler)

--------------------------------------------------------------------------------

data AppContext context = AppContext
  { appDbPool :: HSQL.Pool,
    appTracer :: OTEL.Tracer,
    appSmtpConfig :: Maybe SmtpConfig,
    appHostname :: Hostname,
    appEnvironment :: Environment,
    appLoggerEnv :: Log.LoggerEnv,
    appCustom :: context
  }

instance Has.Has HSQL.Pool (AppContext ctx) where
  getter = appDbPool
  modifier f ctx@AppContext {appDbPool} = ctx {appDbPool = f appDbPool}

instance Has.Has OTEL.Tracer (AppContext ctx) where
  getter = appTracer
  modifier f ctx@AppContext {appTracer} = ctx {appTracer = f appTracer}

instance Has.Has (Maybe SmtpConfig) (AppContext ctx) where
  getter = appSmtpConfig
  modifier f ctx@AppContext {appSmtpConfig} = ctx {appSmtpConfig = f appSmtpConfig}

instance Has.Has Hostname (AppContext ctx) where
  getter = appHostname
  modifier f ctx@AppContext {appHostname} = ctx {appHostname = f appHostname}

instance Has.Has Environment (AppContext ctx) where
  getter = appEnvironment
  modifier f ctx@AppContext {appEnvironment} = ctx {appEnvironment = f appEnvironment}

instance Has.Has Log.LoggerEnv (AppContext ctx) where
  getter = appLoggerEnv
  modifier f ctx@AppContext {appLoggerEnv} = ctx {appLoggerEnv = f appLoggerEnv}

type ServantContext = '[Servant.ErrorFormatters, AuthHandler Wai.Request Authz]
