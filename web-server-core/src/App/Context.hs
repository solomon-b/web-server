{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module App.Context where

--------------------------------------------------------------------------------

import App.Auth (Authz)
import App.Config
import Data.Has qualified as Has
import Data.Kind (Constraint, Type)
import GHC.TypeError (ErrorMessage (Text))
import GHC.TypeLits (TypeError)
import Hasql.Pool qualified as HSQL (Pool)
import Log qualified
import Network.Wai qualified as Wai
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified
import Servant.Server (Context ((:.)))
import Servant.Server.Experimental.Auth (AuthHandler)

--------------------------------------------------------------------------------

data AppContext context = AppContext
  { appDbPool :: HSQL.Pool,
    appTracer :: OTEL.Tracer,
    appTracerProvider :: OTEL.TracerProvider,
    appHostname :: Hostname,
    appEnvironment :: Environment,
    appLoggerEnv :: Log.LoggerEnv,
    appWarpConfig :: WarpConfig,
    appCustom :: context
  }

instance Has.Has HSQL.Pool (AppContext ctx) where
  getter = appDbPool
  modifier f ctx@AppContext {appDbPool} = ctx {appDbPool = f appDbPool}

instance Has.Has OTEL.Tracer (AppContext ctx) where
  getter = appTracer
  modifier f ctx@AppContext {appTracer} = ctx {appTracer = f appTracer}

instance Has.Has Hostname (AppContext ctx) where
  getter = appHostname
  modifier f ctx@AppContext {appHostname} = ctx {appHostname = f appHostname}

instance Has.Has Environment (AppContext ctx) where
  getter = appEnvironment
  modifier f ctx@AppContext {appEnvironment} = ctx {appEnvironment = f appEnvironment}

instance Has.Has Log.LoggerEnv (AppContext ctx) where
  getter = appLoggerEnv
  modifier f ctx@AppContext {appLoggerEnv} = ctx {appLoggerEnv = f appLoggerEnv}

instance Has.Has OTEL.TracerProvider (AppContext ctx) where
  getter = appTracerProvider
  modifier f ctx@AppContext {appTracerProvider} = ctx {appTracerProvider = f appTracerProvider}

instance Has.Has WarpConfig (AppContext ctx) where
  getter = appWarpConfig
  modifier f ctx@AppContext {appWarpConfig} = ctx {appWarpConfig = f appWarpConfig}

type family EndsWith (xs :: [Type]) (needle :: Type) :: Constraint where
  EndsWith '[needle] needle = ()
  EndsWith (_ ': xs) needle = EndsWith xs needle
  EndsWith '[] needle = TypeError ('Text "Context does not end with the required AuthHandler")

appendContext :: Servant.Context xs -> Servant.Context ys -> Servant.Context (Append xs ys)
appendContext Servant.EmptyContext ys = ys
appendContext (x :. xs) ys = x :. appendContext xs ys

-- type family to reflect ++ at the type level
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type AuthContext = AuthHandler Wai.Request Authz

type ServantContext = '[AuthContext]
