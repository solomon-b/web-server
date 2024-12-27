module Lucid.Extras where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid.Base qualified as Lucid

--------------------------------------------------------------------------------
-- HTML5

path_ :: [Lucid.Attributes] -> Lucid.Html () -> Lucid.Html ()
path_ = Lucid.term "path"

ariaHidden_ :: Text -> Lucid.Attributes
ariaHidden_ = Lucid.makeAttributes "aria-hidden"

viewBox_ :: Text -> Lucid.Attributes
viewBox_ = Lucid.makeAttributes "viewBox"

version_ :: Text -> Lucid.Attributes
version_ = Lucid.makeAttributes "version"

d_ :: Text -> Lucid.Attributes
d_ = Lucid.makeAttributes "d"

dataViewComponent_ :: Text -> Lucid.Attributes
dataViewComponent_ = Lucid.makeAttributes "data-view-component"

--------------------------------------------------------------------------------
-- AlpineJS

xData_ :: Text -> Lucid.Attributes
xData_ = Lucid.makeAttributes "x-data"

xModel_ :: Text -> Lucid.Attributes
xModel_ = Lucid.makeAttributes "x-model"

xBindHidden_ :: Text -> Lucid.Attributes
xBindHidden_ = Lucid.makeAttributes "x-bind:hidden"

xOnBlur_ :: Text -> Lucid.Attributes
xOnBlur_ = Lucid.makeAttributes "x-on:blur"

xOnInput_ :: Text -> Lucid.Attributes
xOnInput_ = Lucid.makeAttributes "x-on:input"

xOnChange_ :: Text -> Lucid.Attributes
xOnChange_ = Lucid.makeAttributes "x-on:change"

xOnClick_ :: Text -> Lucid.Attributes
xOnClick_ = Lucid.makeAttributes "x-on:click"

xBindClass_ :: Text -> Lucid.Attributes
xBindClass_ = Lucid.makeAttributes "x-bind:class"

xBindDisabled_ :: Text -> Lucid.Attributes
xBindDisabled_ = Lucid.makeAttributes "x-bind:disabled"

xRef_ :: Text -> Lucid.Attributes
xRef_ = Lucid.makeAttributes "x-ref"

xHtml_ :: Text -> Lucid.Attributes
xHtml_ = Lucid.makeAttributes "x-html"

--------------------------------------------------------------------------------
-- HTMX

hxGet_ :: Text -> Lucid.Attributes
hxGet_ = Lucid.makeAttributes "hx-get"

hxPost_ :: Text -> Lucid.Attributes
hxPost_ = Lucid.makeAttributes "hx-post"

hxSwap_ :: Text -> Lucid.Attributes
hxSwap_ = Lucid.makeAttributes "hx-swap"

hxTarget_ :: Text -> Lucid.Attributes
hxTarget_ = Lucid.makeAttributes "hx-target"

hxPushUrl_ :: Text -> Lucid.Attributes
hxPushUrl_ = Lucid.makeAttributes "hx-push-url"
