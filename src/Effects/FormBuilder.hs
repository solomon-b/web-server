{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.FormBuilder where

--------------------------------------------------------------------------------

import Barbies
import Control.Applicative (Const (..))
import Control.Arrow ((&&&))
import Control.Monad.Identity (Identity (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Display (Display (..), display)
import Htmx.Lucid.Core qualified as Lucid.Htmx
import Htmx.Lucid.Extra qualified as Lucid.Htmx
import Lucid qualified

--------------------------------------------------------------------------------

newtype ToForm m a = ToForm {unToForm :: a -> Lucid.HtmlT m ()}

class (Display (Index b), Name b ~ Text) => FormBuilder b where
  type Index b :: Type
  type Name b :: Type

  routeName :: Name b
  idColumn :: b Identity -> Index b
  rowForm :: (Monad m) => b (ToForm m)
  columnNames :: (Monad m) => b (Const (Lucid.HtmlT m ()))

buildForm :: forall m b. (Monad m, FormBuilder b, TraversableB b, ApplicativeB b) => [b Identity] -> Lucid.HtmlT m ()
buildForm rowData = do
  Lucid.div_ [Lucid.class_ "table-responsive"] $ do
    Lucid.table_ [Lucid.class_ "center"] $ do
      Lucid.caption_ "User Table"
      Lucid.thead_ $ do
        Lucid.tr_ $ do
          bfoldMap (Lucid.th_ . getConst) (columnNames @b)
      Lucid.tr_ $ do
        let mkRow = bfoldMap getConst . bzipWith (\(ToForm f) (Identity a) -> Const (f a)) (rowForm @b @m)
        let mkButtons :: Text -> Lucid.HtmlT m ()
            mkButtons x = Lucid.td_ (Lucid.button_ [Lucid.Htmx.hxDelete_ $ routeName @b <> "/" <> x <> "/delete", Lucid.Htmx.hxConfirm_ "Are you sure?", Lucid.Htmx.hxSwap_ "outerHTML"] "❌")
        foldMap (uncurry (<>) . (mkRow &&& (mkButtons . display . idColumn))) rowData
