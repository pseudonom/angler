{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Applicative          ((<$>))
import           Data.Foldable                (foldMap)
import qualified Data.List                    as L (head)
import           Data.Monoid                  ((<>))
import           Prelude                      hiding (head, init, last, length,
                                               tail, zipWith)
import           System.Console.GetOpt        (ArgDescr (..), ArgOrder (..),
                                               OptDescr (..), getOpt)
import           System.Environment           (getArgs)

import           Data.HashMap.Strict          (HashMap)
import           Data.Text                    (Text, isInfixOf)
import qualified Data.Text                    as T (head, tail)
import           Data.Text.Encoding           (decodeUtf8)
import           Data.Text.Lazy               (toStrict)
import           Data.Vector                  (Vector, head, length, tail)
import           Happstack.Lite               (Browsing (..), Method (..),
                                               Response, ServerPart,
                                               asContentType,
                                               defaultServerConfig, dir,
                                               getHeaderM, lookText, method,
                                               msum, ok, port, serve,
                                               serveDirectory, serveFile,
                                               setHeaderM, toResponse)
import           Happstack.Server.Compression (compressedResponseFilter)
import           Text.Blaze.Html5             (Html, toHtml, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

import           Analyze                      (BadLang (..), Fail (..),
                                               GoodLang (..), InLang (..),
                                               TextInfo (..), WordInfo (..),
                                               analyze)
import           Etym                         (Lang (..), Origin, Target,
                                               loadEtyms)
import           Thesaurus                    (Thes, loadThes)

main :: IO ()
main = do
  p <- L.head . (\(a, _, _) -> a) . getOpt Permute [heroku] <$> getArgs
  e <- loadEtyms "data/etym.csv"
  t <- loadThes "data/moby-thesaurus.dict"
  serve (Just defaultServerConfig { port = p }) $ app e t

heroku :: OptDescr Int
heroku = Option ['p'] [] (ReqArg read "PORT") "Port to serve"

app :: HashMap Target Origin -> Thes -> ServerPart Response
app e t = msum
  [ dir "static" $ do
       expire
       serveDirectory DisableBrowsing [] "static"
  , dir "about" $ do
       expire
       serveFile (asContentType "text/html") "static/about.html"
  , angler e t
  ]

expire :: ServerPart ()
expire = setHeaderM "Cache-Control" "max-age=86400"

data IsWebKit = WebKit | NotWebKit

angler :: HashMap Target Origin -> Thes -> ServerPart Response
angler e t = msum [input, output] where
  input = do method GET
             expire
             serveFile (asContentType "text/html") "static/input.html"
  output = do method POST
              _ <- compressedResponseFilter
              text <- toStrict <$> lookText "text"
              wk <- renderer <$> getHeaderM "User-Agent"
              ok . template . htmlify wk $ angle e t text where
                renderer Nothing = NotWebKit
                renderer (Just ua) | "WebKit" `isInfixOf` decodeUtf8 ua = WebKit
                                   | otherwise = NotWebKit

template :: Html -> Response
template body = toResponse $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title "Angler"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "static/angler.min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href
        "http://fonts.googleapis.com/css?family=Almendra"
      H.script ! A.type_ "text/javascript" $ js
    H.body ! A.class_ "marginalia" $ body

htmlify :: IsWebKit -> Vector TextInfo -> Html
htmlify wk ts = H.article ! A.class_ (cc wk) $ spanify first (head ts) <> foldMap (spanify toHtml) (tail ts) where
  -- NBSP ensures equal line-height for drop cap
  spanify _ (NonWord "\r\n") = H.br >> H.span " "
  spanify h (NonWord t) = h t
  spanify h (WordLike (WordInfo o (Left NoSyn))) =
    anno "no-syn" $ h o
  spanify h (WordLike (WordInfo o (Left NotBad))) =
    anno "not-lat" $ h o
  spanify h (WordLike (WordInfo o (Left NoEtym))) =
    anno "no-etym" $ h o
  spanify _ (WordLike (WordInfo o (Right syns))) =
    H.span ! A.class_ "select" ! A.tabindex "0" $ do
      H.ul ! A.class_ "tooltip" $
        optify "option current original" o <>
        foldMap (optify "option") syns
      H.span ! A.class_ "selection" $ toHtml o where
        optify cl t = H.li ! A.class_ cl ! H.dataAttribute "value" (H.toValue t) $
                      toHtml t
  cc WebKit = "single-column"
  cc NotWebKit = if length ts > 400 then "multi-column" else "single-column"
  first w = do
    H.span ! A.class_ "first-letter" $ toHtml $ T.head w
    H.span . toHtml $ T.tail w
  anno cl o = H.span ! A.tabindex "0" ! A.class_ cl $ o

angle :: HashMap Target Origin -> Thes -> Text -> Vector TextInfo
angle e s = analyze e s (InLang $ Lang "Eng")
                        (BadLang $ Lang "Lat")
                        (GoodLang $ Lang "Ang")

js :: Html
js = "'use strict';window.onload=function(){var c=document.querySelectorAll(\"li\");Array.prototype.forEach.call(c,function(a){a.onclick=function(){a.parentNode.parentNode.querySelectorAll(\".selection\")[0].textContent=a.getAttribute(\"data-value\");var b=a.parentNode.querySelectorAll(\".option.current\")[0];b.classList?b.classList.remove(\"current\"):b.className=b.className.replace(/(^|\b)current(\b|$)/gi,\" \");a.classList?a.classList.add(\"current\"):a.className+=\" current\"}})};"
