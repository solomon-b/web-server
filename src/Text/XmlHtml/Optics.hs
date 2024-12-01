-- | Lensy Interface for 'Text.XmlHtml
--
-- For all examples we will use the following HTML:
--
-- <!DOCTYPE html>
--   <html>
--     <head>
--     </head>
--     <body>
--       <div id='main'>
--         <div class='foo'>hello</div>
--         <div class='bar'>
--           <div class='foo'>
--             world
--           </div>
--           <ul>
--             <li id='home-tab'>One</li>
--             <li id='focused'>Two</li>
--             <li>Three</li>
--           </ul>
--         </div>
--       </div>
--     </body>
-- </html>
module Text.XmlHtml.Optics where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Text.XmlHtml

--------------------------------------------------------------------------------
-- Document

_XmlDocument :: Prism' Document (Encoding, Maybe DocType, [Node])
_XmlDocument = prism' into out
  where
    into (x, y, z) = XmlDocument x y z
    out = \case
      XmlDocument x y z -> Just (x, y, z)
      _ -> Nothing

_HtmlDocument :: Prism' Document (Encoding, Maybe DocType, [Node])
_HtmlDocument = prism' into out
  where
    into (x, y, z) = HtmlDocument x y z
    out = \case
      HtmlDocument x y z -> Just (x, y, z)
      _ -> Nothing

_docContent :: Lens' Document [Node]
_docContent = lens get setter
  where
    get = \case
      XmlDocument _ _ nodes -> nodes
      HtmlDocument _ _ nodes -> nodes
    setter doc nodes =
      case doc of
        XmlDocument x y _ -> XmlDocument x y nodes
        HtmlDocument x y _ -> HtmlDocument x y nodes

_docContent' :: Traversal' Document Node
_docContent' = _docContent . traversed

--------------------------------------------------------------------------------
-- Node

_TextNode :: Traversal' Node Text
_TextNode f = \case
  TextNode txt -> f txt <&> TextNode
  n -> pure n

_Comment :: Traversal' Node Text
_Comment f = \case
  Comment txt -> f txt <&> Comment
  n -> pure n

data FocusedElement = FocusedElement {elTag :: Text, elAttributes :: [(Text, Text)], elChildren :: [Node]}
  deriving (Show)

-- | Focus on the @Element@ constructors of a 'Node'.
_FocusedElement :: Traversal' Node FocusedElement
_FocusedElement f = \case
  Element t a ns -> f (FocusedElement t a ns) <&> \(FocusedElement t' a' ns') -> Element t' a' ns'
  n -> pure n

_elTag :: Lens' FocusedElement Text
_elTag = lens get setter
  where
    get = elTag
    setter el t = el {elTag = t}

_elAttributes :: Lens' FocusedElement [(Text, Text)]
_elAttributes = lens get setter
  where
    get = elAttributes
    setter el as = el {elAttributes = as}

_elChildren :: Lens' FocusedElement [Node]
_elChildren = lens get setter
  where
    get = elChildren
    setter el c = el {elChildren = c}

_elChildren' :: Traversal' FocusedElement Node
_elChildren' = _elChildren . traversed

--------------------------------------------------------------------------------
-- DocType

root :: Lens' DocType Text
root = lens get setter
  where
    get (DocType r _ _) = r
    setter (DocType _ eid is) r = DocType r eid is

_ExternalID :: Lens' DocType ExternalID
_ExternalID = lens get setter
  where
    get (DocType _ eid _) = eid
    setter (DocType r _ is) eid = DocType r eid is

_InternalSubset :: Lens' DocType InternalSubset
_InternalSubset = lens get setter
  where
    get (DocType _ _ is) = is
    setter (DocType r eid _) = DocType r eid

--------------------------------------------------------------------------------
-- ExternalID

_Public :: Prism' ExternalID (Text, Text)
_Public = prism' into out
  where
    into (x, y) = Public x y
    out = \case
      Public x y -> Just (x, y)
      _ -> Nothing

_System :: Prism' ExternalID Text
_System = prism' into out
  where
    into = System
    out = \case
      System x -> Just x
      _ -> Nothing

_NoExternalID :: Prism' ExternalID ()
_NoExternalID = prism' into out
  where
    into () = NoExternalID
    out = \case
      NoExternalID -> Just ()
      _ -> Nothing

--------------------------------------------------------------------------------
-- InternalSubset

_InternalText :: Prism' InternalSubset Text
_InternalText = prism' into out
  where
    into = InternalText
    out = \case
      InternalText x -> Just x
      _ -> Nothing

_NoInternalSubset :: Prism' InternalSubset ()
_NoInternalSubset = prism' into out
  where
    into () = NoInternalSubset
    out = \case
      NoInternalSubset -> Just ()
      _ -> Nothing

--------------------------------------------------------------------------------
-- Encoding

_UTF8 :: Prism' Encoding ()
_UTF8 = prism' into out
  where
    into () = UTF8
    out = \case
      UTF8 -> Just ()
      _ -> Nothing

_UTF16BE :: Prism' Encoding ()
_UTF16BE = prism' into out
  where
    into () = UTF16BE
    out = \case
      UTF16BE -> Just ()
      _ -> Nothing

_UTF16LE :: Prism' Encoding ()
_UTF16LE = prism' into out
  where
    into () = UTF16LE
    out = \case
      UTF16LE -> Just ()
      _ -> Nothing

_ISO_8859_1 :: Prism' Encoding ()
_ISO_8859_1 = prism' into out
  where
    into () = ISO_8859_1
    out = \case
      ISO_8859_1 -> Just ()
      _ -> Nothing

--------------------------------------------------------------------------------
-- RenderOptions

_roAttributeSurround :: Lens' RenderOptions AttrSurround
_roAttributeSurround = lens get setter
  where
    get = roAttributeSurround
    setter ro as = ro {roAttributeSurround = as}

_roAttributeResolveInternal :: Lens' RenderOptions AttrResolveInternalQuotes
_roAttributeResolveInternal = lens get setter
  where
    get = roAttributeResolveInternal
    setter ro ari = ro {roAttributeResolveInternal = ari}

_roExplicitEmptyAttrs :: Lens' RenderOptions (Maybe (HashMap Text (HashSet Text)))
_roExplicitEmptyAttrs = lens get setter
  where
    get = roExplicitEmptyAttrs
    setter ro eea = ro {roExplicitEmptyAttrs = eea}

--------------------------------------------------------------------------------
-- AttrSurround

_SurroundDoubleQuote :: Prism' AttrSurround ()
_SurroundDoubleQuote = prism' into out
  where
    into () = SurroundDoubleQuote
    out = \case
      SurroundDoubleQuote -> Just ()
      _ -> Nothing

_SurroundSingleQuote :: Prism' AttrSurround ()
_SurroundSingleQuote = prism' into out
  where
    into () = SurroundSingleQuote
    out = \case
      SurroundSingleQuote -> Just ()
      _ -> Nothing

--------------------------------------------------------------------------------
-- AttrResolveInternalQuotes

_AttrResolveByEscape :: Prism' AttrResolveInternalQuotes ()
_AttrResolveByEscape = prism' into out
  where
    into () = AttrResolveByEscape
    out = \case
      AttrResolveByEscape -> Just ()
      _ -> Nothing

_AttrResolveAvoidEscape :: Prism' AttrResolveInternalQuotes ()
_AttrResolveAvoidEscape = prism' into out
  where
    into () = AttrResolveAvoidEscape
    out = \case
      AttrResolveAvoidEscape -> Just ()
      _ -> Nothing

--------------------------------------------------------------------------------
-- HTML Specific Derived Lenses

-- | Traversal to find the first 'Element' with a given tag in a Node tree
_el :: Text -> Traversal' Node FocusedElement
_el tag = deepOf (_FocusedElement . _elChildren . traversed) (_FocusedElement . filtered (\el -> elTag el == tag))

-- | Focus on the HTML @<head></head>@ tag.
--
-- > preview (_docContent' . Text.XmlHtml.Optics._head) xml
-- Just (FocusedElement {elTag = "head", elAttributes = [], elChildren = [TextNode "\n    "]})
_head :: Traversal' Node FocusedElement
_head = _el "head"

-- | Focus on the HTML @<body></body>@ tag.
--
-- > preview (_docContent' . _body) xml
-- Just (FocusedElement {elTag = "body", elAttributes = [], elChildren = [TextNode "\n      ",Element {elementTag = "div", elementAttrs = [("id","main")], elementChildren = [TextNode "\n        ",Element {elementTag = "div", elementAttrs = [("class","foo")], elementChildren = [TextNode "hello"]},TextNode "\n        ",Element {elementTag = "div", elementAttrs = [("class","bar")], elementChildren = [TextNode "\n          ",Element {elementTag = "div", elementAttrs = [("class","foo")], elementChildren = [TextNode "\n            world\n          "]},TextNode "\n          ",Element {elementTag = "ul", elementAttrs = [], elementChildren = [TextNode "\n            ",Element {elementTag = "li", elementAttrs = [], elementChildren = [TextNode "One"]},TextNode "\n            ",Element {elementTag = "li", elementAttrs = [("id","focused")], elementChildren = [TextNode "Two"]},TextNode "\n            ",Element {elementTag = "li", elementAttrs = [], elementChildren = [TextNode "Three"]},TextNode "\n          "]},TextNode "\n        "]},TextNode "\n      "]},TextNode "\n    "]})
_body :: Traversal' Node FocusedElement
_body = _el "body"

_navbar :: Traversal' Node FocusedElement
_navbar = _el "navbar"

_main :: Traversal' Node FocusedElement
_main = _el "main"

_a :: Traversal' Node FocusedElement
_a = _el "a"

_div :: Traversal' Node FocusedElement
_div = _el "div"

_ul :: Traversal' Node FocusedElement
_ul = _el "ul"

_li :: Traversal' Node FocusedElement
_li = _el "li"

_attr :: (Text, Text) -> Traversal' Node FocusedElement
_attr attr = deepOf (_FocusedElement . _elChildren . traversed) (_FocusedElement . filtered (\el -> attr `elem` elAttributes el))

_id :: Text -> Traversal' Node FocusedElement
_id val = _attr ("id", val)

_class :: Text -> Traversal' Node FocusedElement
_class val = _attr ("class", val)

_any :: Traversal' Node FocusedElement
_any = deepOf (_FocusedElement . _elChildren . traversed) (_FocusedElement . filtered (const True))

-- | Focus through a nested series of element tags
path :: [Text] -> Traversal' Node FocusedElement
path = foldr ((\outer inner -> outer . _elChildren . traversed . inner) . _el) _any

--------------------------------------------------------------------------------
-- hx-swap style helpers

-- | Replace the inner html of the target element(s)
swapInner :: Traversal' Node FocusedElement -> [Node] -> Document -> Document
swapInner t = set (_docContent' . t . _elChildren)

-- | Modify the inner html of the target element(s)
modifyInner :: Traversal' Node FocusedElement -> ([Node] -> [Node]) -> Document -> Document
modifyInner t = over (_docContent' . t . _elChildren)

-- | Replace the entire target element(s)
swapOuter :: Traversal' Node FocusedElement -> FocusedElement -> Document -> Document
swapOuter t = set (_docContent' . t)

-- | Modify the entire target element(s)
modifyOuter :: Traversal' Node FocusedElement -> (FocusedElement -> FocusedElement) -> Document -> Document
modifyOuter t = over (_docContent' . t)

-- | Replace the text content of the target element(s)
swapTextContent :: Traversal' Node FocusedElement -> Text -> Document -> Document
swapTextContent t = set (_docContent' . t . _elChildren' . _TextNode)

-- | Replace the text content of the target element(s)
modifyTextContent :: Traversal' Node FocusedElement -> (Text -> Text) -> Document -> Document
modifyTextContent t = over (_docContent' . t . _elChildren' . _TextNode)

-- | Insert the response before the target element
-- insertBeforeBegin :: Traversal' Node FocusedElement -> Node -> Document -> Document
-- insertBeforeBegin = _

-- | Insert the 'Node' before the first child of the target element
insertAfterBegin :: Traversal' Node FocusedElement -> Node -> Document -> Document
insertAfterBegin t n = over (_docContent' . t . _elChildren) (n :)

-- | Insert the response after the last child of the target element
insertBeforeEnd :: Traversal' Node FocusedElement -> Node -> Document -> Document
insertBeforeEnd t n = over (_docContent' . t . _elChildren) (<> [n])

-- | Insert the response after the target element(s)
-- insertAfterEnd :: Traversal' Node FocusedElement -> Node -> Document -> Document
-- insertAfterEnd t = _

-- | Deletes the target element regardless of the response
-- delete :: Traversal' Node FocusedElement -> Document -> Document
-- delete = _

-- | HTML Example
xml :: Document
xml = either undefined id $ parseHTML "" "<!DOCTYPE html>\n  <html>\n    <head>\n    </head>\n    <body>\n      <div id='main'>\n        <div class='foo'>hello</div>\n        <div class='bar'>\n          <div class='foo'>\n            world\n          </div>\n          <ul>\n            <li id='home-tab'><a class='derp'>One</a></li>\n            <li id='focused'>Two</li>\n            <li>Three</li>\n          </ul>\n        </div>\n      </div>\n    </body>\n</html>\n"

ex :: Document -> Document
ex node = node & transformOnOf _docContent' (_FocusedElement . _elChildren') (over (_el "div") (over _elChildren (const [TextNode "UPDATED!!"])))
