{-# LANGUAGE KindSignatures #-}

module Effects.Database.Utils where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Text qualified as Text
import Effects.Database.Class
import Errors (throw500)
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 qualified

--------------------------------------------------------------------------------
-- Working with Models and Domains

class ModelParser (model :: (Type -> Type) -> Type) (domain :: Type) where
  parseModel :: model Rel8.Result -> domain

class ModelPrinter (model :: (Type -> Type) -> Type) (domain :: Type) where
  printModel :: domain -> model Rel8.Expr

--------------------------------------------------------------------------------
-- Query Execution

execQuerySpan ::
  ( Log.MonadLog m,
    MonadDB m
  ) =>
  HSQL.Statement () result ->
  m (Either HSQL.UsageError result)
execQuerySpan statement@(HSQL.Statement bs _ _ _) = do
  Log.logInfo "db query" $ Text.pack $ Char8.unpack bs
  execStatement statement

execQuerySpanThrow ::
  ( Log.MonadLog m,
    Show result,
    MonadDB m,
    MonadThrow m
  ) =>
  HSQL.Statement () result ->
  m result
execQuerySpanThrow statement = do
  execQuerySpan statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throw500 "Something went wrong"
    Right res -> pure res

execQuerySpanThrowMessage ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  BL.ByteString ->
  HSQL.Statement () result ->
  m result
execQuerySpanThrowMessage msg statement = do
  execQuerySpan statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throw500 msg
    Right res -> pure res

execQuerySpanThrowMessage' ::
  ( Functor f,
    Log.MonadLog m,
    MonadDB m,
    ModelParser model domain,
    MonadThrow m
  ) =>
  BL.ByteString ->
  HSQL.Statement () (f (model Rel8.Result)) ->
  m (f domain)
execQuerySpanThrowMessage' msg statement =
  fmap parseModel <$> execQuerySpanThrowMessage msg statement
