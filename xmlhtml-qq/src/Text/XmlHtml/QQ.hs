{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
This module modifies and re-uses code from:

- https://github.com/cdepillabout/xml-html-qq
Copyright Dennis Gosnell (c) 2017

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

- https://hackage.haskell.org/package/interpolatedstring-qq
Copyright (c) Erik Charlebois.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The names of the author may not be used to endorse or promote
   products derived from this software without specific prior written
   permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
-}
module Text.XmlHtml.QQ (html, html', node') where

--------------------------------------------------------------------------------

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Instances.TH.Lift ()
import Language.Haskell.Meta.Parse
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax qualified as TH.Syntax
import Text.XmlHtml (AttrResolveInternalQuotes, AttrSurround, DocType, Document (..), Encoding, ExternalID, InternalSubset, Node, RenderOptions)
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------

$( deriveLiftMany
     [ ''AttrResolveInternalQuotes,
       ''AttrSurround,
       ''RenderOptions,
       ''ExternalID,
       ''InternalSubset,
       ''Encoding,
       ''DocType,
       ''Node,
       ''Document
     ]
 )

-- | Create a 'QuasiQuoter' for 'Exp's.
createExpQuasiQuoter ::
  -- | The function to use for 'QuasiQuoter's 'quoteExp'.
  (String -> Q Exp) ->
  QuasiQuoter
createExpQuasiQuoter f =
  QuasiQuoter
    { quoteExp = f,
      quotePat = error "not used",
      quoteType = error "not used",
      quoteDec = error "not used"
    }

-- | This function handles errors that occur when a 'Document' can't be parsed.
--
-- This function throws an 'error' with an explanation of what happened.
handleParseDocErr ::
  -- | The type of a document that was being parsed.  Should either be
  -- @\"XML\"@ or @\"HTML\"@.
  String ->
  -- | The name of the function that was being used to parse the document.
  -- Should probably either be @\"Text.XML.parseText\"@ or
  -- @\"Text.HTML.DOM.parseLT\"@ depending on whether you're parsing XML or
  -- HTML.
  String ->
  -- | The actual XML or HTML string that you were trying to parse into a
  -- 'Document'.
  String ->
  -- | The exception that occurred when trying to parse the 'Document'.
  String ->
  a
handleParseDocErr docType parseFunction string exception =
  let msg =
        "ERROR: Trying to parse a string into an "
          `mappend` docType
          `mappend` " Document,\n"
          `mappend` "but got the following error from "
          `mappend` parseFunction
          `mappend` ":\n"
          `mappend` exception
          `mappend` "\n"
          `mappend` "attempting to parse the following document:\n"
          `mappend` string
   in error msg

--------------------------------------------------------------------------------

data StringPart = Literal String | AntiQuote String deriving (Show)

parseHaskell :: String -> String -> [StringPart]
parseHaskell a [] = [Literal (reverse a)]
parseHaskell a ('\\' : x : xs) = parseHaskell (x : a) xs
parseHaskell a ['\\'] = parseHaskell ('\\' : a) []
parseHaskell a ('}' : xs) = AntiQuote (reverse a) : parseStr [] xs
parseHaskell a (x : xs) = parseHaskell (x : a) xs

parseStr :: String -> String -> [StringPart]
parseStr a [] = [Literal (reverse a)]
parseStr a ('\\' : x : xs) = parseStr (x : a) xs
parseStr a ['\\'] = parseStr ('\\' : a) []
parseStr a ('#' : '{' : xs) = Literal (reverse a) : parseHaskell [] xs
parseStr a (x : xs) = parseStr (x : a) xs

makeExpr :: [StringPart] -> Q Exp
makeExpr [] = [|""|]
makeExpr ((Literal a) : xs) = TH.appE [|(++) a|] (makeExpr xs)
makeExpr ((AntiQuote a) : xs) = TH.appE [|(++) (trimQuotes (show $(reifyStringToHaskell a)))|] (makeExpr xs)

trimQuotes :: String -> String
trimQuotes s = reverse $ dropWhile (== '"') $ reverse $ dropWhile (== '"') s

reifyStringToHaskell :: String -> Q Exp
reifyStringToHaskell s =
  case parseExp s of
    Left err -> TH.reportError err >> [|""|]
    Right exp' -> return exp'

rstrExp :: String -> Q Exp
rstrExp s = makeExpr $ parseStr [] $ filter (/= '\r') s

-- | Interpolates Haskell expressions then parses to @Either String Document@.
html :: QuasiQuoter
html =
  createExpQuasiQuoter $ \string ->
    TH.appE [|Xml.parseHTML "index.html" . Char8.strip . Char8.pack|] $ rstrExp string

-- | Parses HTML 'Document' terms. Does not allow for interpolation.
html' :: QuasiQuoter
html' =
  createExpQuasiQuoter $ \string ->
    let eitherDoc = Xml.parseHTML "index.html" $ Char8.strip $ Char8.pack string
     in either
          (handleParseDocErr "XML" "Text.XML.parseText" string)
          TH.Syntax.lift
          eitherDoc

-- | Parses a single 'Xml.Node' from a 'BS.ByteString'.
parseNode :: BS.ByteString -> Either String Xml.Node
parseNode bs =
  Xml.parseHTML "index.html" (Char8.strip bs) >>= \case
    Xml.XmlDocument {} -> Left "Didn't parse as HTML"
    Xml.HtmlDocument {docContent} | length docContent == 1 -> Right $ head docContent
    Xml.HtmlDocument {docContent} -> Left $ "Too many nodes in: " <> show docContent

-- | Parses an HTML 'Node'. Does not allow for interpolation.
node' :: QuasiQuoter
node' =
  createExpQuasiQuoter $ \string ->
    let eitherNode = parseNode $ Char8.pack string
     in either
          error -- (handleParseDocErr "XML" "Text.XML.parseText" string)
          TH.Syntax.lift
          eitherNode
