{-# LANGUAGE QuasiQuotes #-}

module Test.Text.XmlHtml.Optics where

--------------------------------------------------------------------------------

import Control.Lens
import Data.ByteString
import Data.String.Interpolate (i)
import Test.Hspec qualified as Hspec
import Text.XmlHtml qualified as Xml
import Text.XmlHtml.Optics

--------------------------------------------------------------------------------

template :: ByteString
template =
  [i|<!DOCTYPE html>
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
        </div>
      </div>
    </body>
</html>|]

pageNode :: Xml.Node
pageNode =
  Xml.Element
    { elementTag = "html",
      elementAttrs = [],
      elementChildren =
        [ Xml.TextNode "\n    ",
          Xml.Element
            { elementTag = "head",
              elementAttrs = [],
              elementChildren = [Xml.TextNode "\n    "]
            },
          Xml.TextNode "\n    ",
          bodyNode,
          Xml.TextNode "\n"
        ]
    }

bodyNode :: Xml.Node
bodyNode =
  Xml.Element
    { elementTag = "body",
      elementAttrs = [],
      elementChildren =
        [ Xml.TextNode "\n      ",
          Xml.Element
            { elementTag = "div",
              elementAttrs = [("id", "main")],
              elementChildren =
                [ Xml.TextNode "\n        ",
                  Xml.Element
                    { elementTag = "div",
                      elementAttrs = [("class", "foo")],
                      elementChildren = [Xml.TextNode "hello"]
                    },
                  Xml.TextNode "\n        ",
                  Xml.Element
                    { elementTag = "div",
                      elementAttrs = [("class", "bar")],
                      elementChildren =
                        [ Xml.TextNode "\n          ",
                          Xml.Element
                            { elementTag = "div",
                              elementAttrs = [("class", "foo")],
                              elementChildren = [Xml.TextNode "\n            world\n          "]
                            },
                          Xml.TextNode "\n          ",
                          Xml.Element
                            { elementTag = "ul",
                              elementAttrs = [],
                              elementChildren =
                                [ Xml.TextNode "\n            ",
                                  Xml.Element
                                    { elementTag = "li",
                                      elementAttrs = [("id", "home-tab")],
                                      elementChildren = [Xml.TextNode "One"]
                                    },
                                  Xml.TextNode "\n            ",
                                  Xml.Element
                                    { elementTag = "li",
                                      elementAttrs = [("id", "focused")],
                                      elementChildren = [Xml.TextNode "Two"]
                                    },
                                  Xml.TextNode "\n            ",
                                  Xml.Element
                                    { elementTag = "li",
                                      elementAttrs = [],
                                      elementChildren = [Xml.TextNode "Three"]
                                    },
                                  Xml.TextNode "\n          "
                                ]
                            },
                          Xml.TextNode "\n        "
                        ]
                    },
                  Xml.TextNode "\n      "
                ]
            },
          Xml.TextNode "\n    "
        ]
    }

--------------------------------------------------------------------------------

spec :: Hspec.Spec
spec = do
  Hspec.describe "_docContent" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = preview (_Just . _docContent) page
      page' `Hspec.shouldBe` Just [pageNode]

  Hspec.describe "_docContent'" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = preview (_Just . _docContent') page
      page' `Hspec.shouldBe` Just pageNode

  Hspec.describe "_el" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = preview (_Just . _docContent' . _el "body") page
      page' `Hspec.shouldBe` preview _FocusedElement bodyNode

  Hspec.describe "_attr" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = preview (_Just . _docContent' . _attr ("id", "home-tab")) page
          expected = FocusedElement {elTag = "li", elAttributes = [("id", "home-tab")], elChildren = [Xml.TextNode "One"]}
      page' `Hspec.shouldBe` Just expected

  Hspec.describe "_attr'" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = preview (_Just . _docContent' . _FocusedElement . _attr' ("id", "home-tab")) page
          expected = FocusedElement {elTag = "li", elAttributes = [("id", "home-tab")], elChildren = [Xml.TextNode "One"]}
      page' `Hspec.shouldBe` Just expected

  Hspec.describe "path'" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = preview (_Just . _docContent' . path ["html", "body", "div"]) page
          expected = FocusedElement {elTag = "div", elAttributes = [("class", "foo")], elChildren = [Xml.TextNode "hello"]}
      page' `Hspec.shouldBe` Just expected

  Hspec.describe "swapInner" $ do
    Hspec.it "preview" $ do
      let page = eitherToMaybe $ Xml.parseHTML "index.html" template
          page' = swapInner (_id "main") [Xml.TextNode "hello"] <$> page
          expected =
            Xml.HtmlDocument
              Xml.UTF8
              (Just (Xml.DocType "html" Xml.NoExternalID Xml.NoInternalSubset))
              [ Xml.Element
                  { elementTag = "html",
                    elementAttrs = [],
                    elementChildren =
                      [ Xml.TextNode "\n    ",
                        Xml.Element
                          { elementTag = "head",
                            elementAttrs = [],
                            elementChildren = [Xml.TextNode "\n    "]
                          },
                        Xml.TextNode "\n    ",
                        Xml.Element
                          { elementTag = "body",
                            elementAttrs = [],
                            elementChildren =
                              [ Xml.TextNode "\n      ",
                                Xml.Element
                                  { elementTag = "div",
                                    elementAttrs = [("id", "main")],
                                    elementChildren = [Xml.TextNode "hello"]
                                  },
                                Xml.TextNode "\n    "
                              ]
                          },
                        Xml.TextNode "\n"
                      ]
                  }
              ]
      page' `Hspec.shouldBe` Just expected

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
