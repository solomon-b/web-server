{-# LANGUAGE QuasiQuotes #-}

module Test.Text.XmlHtml.Optics where

--------------------------------------------------------------------------------

import Control.Lens
import Test.Hspec qualified as Hspec
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics
import Text.XmlHtml.QQ (html', node')

--------------------------------------------------------------------------------

template :: Xml.Document
template =
  [html'|<!DOCTYPE html>
  <html>
    <head>
    </head>
    <body>
      <div id='main'>
        <div class='foo'>hello</div>
        <div class='bar'>
          <div class='foo'>
            world
          </div>
          <ul>
            <li id='home-tab'>One</li>
            <li id='focused'>Two</li>
            <li>Three</li>
          </ul>
          <ol class="ol-class">
            <li>First</li>
            <li>Second</li>
            <li>Third</li>
          </ol>
        </div>
      </div>
    </body>
</html>
|]

pageNode :: Xml.Node
pageNode =
  [node'|
  <html>
    <head>
    </head>
    <body>
      <div id='main'>
        <div class='foo'>hello</div>
        <div class='bar'>
          <div class='foo'>
            world
          </div>
          <ul>
            <li id='home-tab'>One</li>
            <li id='focused'>Two</li>
            <li>Three</li>
          </ul>
          <ol class="ol-class">
            <li>First</li>
            <li>Second</li>
            <li>Third</li>
          </ol>
        </div>
      </div>
    </body>
</html>
|]

bodyNode :: Xml.Node
bodyNode =
  [node'|
    <body>
      <div id='main'>
        <div class='foo'>hello</div>
        <div class='bar'>
          <div class='foo'>
            world
          </div>
          <ul>
            <li id='home-tab'>One</li>
            <li id='focused'>Two</li>
            <li>Three</li>
          </ul>
          <ol class="ol-class">
            <li>First</li>
            <li>Second</li>
            <li>Third</li>
          </ol>
        </div>
      </div>
    </body>
|]

--------------------------------------------------------------------------------

spec :: Hspec.Spec
spec = do
  Hspec.describe "_docContent" $ do
    Hspec.it "preview" $ do
      let page = view _docContent template
      page `Hspec.shouldBe` [pageNode]

  Hspec.describe "_docContent'" $ do
    Hspec.it "preview" $ do
      let page = preview _docContent' template
      page `Hspec.shouldBe` Just pageNode

  Hspec.describe "_el" $ do
    Hspec.it "preview" $ do
      let page = preview (_docContent' . _el "body") template
      page `Hspec.shouldBe` preview _FocusedElement bodyNode

  Hspec.describe "_el" $ do
    Hspec.it "preview" $ do
      let page = preview (_docContent' . _el "ol") template
          expected =
            [node'|
          <ol class="ol-class">
            <li>First</li>
            <li>Second</li>
            <li>Third</li>
          </ol>|]
              & preview _FocusedElement
      page `Hspec.shouldBe` expected

  Hspec.describe "preview _elAttributes" $ do
    Hspec.it "preview" $ do
      let page = preview (_docContent' . _el "ol" . _elAttributes) template
          expected = [("class", "ol-class")]
      page `Hspec.shouldBe` Just expected

  Hspec.describe "set _elAttributes" $ do
    Hspec.it "preview" $ do
      let page = template & set (_docContent' . _el "ol" . _elAttributes) [("class", "new-ol-class")]
          expected = Just [("class", "new-ol-class")]
      preview (_docContent' . _el "ol" . _elAttributes) page `Hspec.shouldBe` expected

  Hspec.describe "over ol class" $ do
    Hspec.it "preview" $ do
      let page = template & over (_docContent' . _el "ol" . _elAttributes . traversed . filtered (\(k, _) -> k == "class") . _2) (<> " new-ol-class")
          expected = Just [("class", "ol-class new-ol-class")]
      preview (_docContent' . _el "ol" . _elAttributes) page `Hspec.shouldBe` expected

  -- let pageFragment' = pageFragment & over (traversed . _el "ol" . _elAttributes . traversed . filtered (\(k, _) -> k == "class") . _2) (<> "max-w-md space-y-1 text-gray-500 list-decimal list-inside ")
  Hspec.describe "_attr" $ do
    Hspec.it "preview" $ do
      let page = preview (_docContent' . _attr ("id", "home-tab")) template
          expected = FocusedElement {elTag = "li", elAttributes = [("id", "home-tab")], elChildren = [Xml.TextNode "One"]}
      page `Hspec.shouldBe` Just expected

  Hspec.describe "_attr'" $ do
    Hspec.it "preview" $ do
      let page = preview (_docContent' . _FocusedElement . _attr' ("id", "home-tab")) template
          expected = FocusedElement {elTag = "li", elAttributes = [("id", "home-tab")], elChildren = [Xml.TextNode "One"]}
      page `Hspec.shouldBe` Just expected

  Hspec.describe "path'" $ do
    Hspec.it "preview" $ do
      let page = preview (_docContent' . path ["html", "body", "div"]) template
          expected = FocusedElement {elTag = "div", elAttributes = [("class", "foo")], elChildren = [Xml.TextNode "hello"]}
      page `Hspec.shouldBe` Just expected

  Hspec.describe "swapInner" $ do
    Hspec.it "preview" $ do
      let page = swapInner (_id "main") [Xml.TextNode "hello"] template
          expected =
            [html'|<!DOCTYPE html>
<html>
    <head>
    </head>
    <body>
      <div id='main'>hello</div>
    </body>
</html>
|]
      page `Hspec.shouldBe` expected

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
