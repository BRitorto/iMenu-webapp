{-# LANGUAGE OverloadedStrings #-}

module Views.Utils
    ( blaze
    , pet
    , layout
    ) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), Html, docTypeHtml, head, title, meta, link, body)
import Text.Blaze.Internal (preEscapedText)
import Web.Scotty (ActionM, html)
import Text.Blaze.Html5.Attributes (href, charset, rel)

pet = preEscapedText

blaze :: Html -> ActionM ()
blaze = html . renderHtml

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
  Text.Blaze.Html5.head $ do
    title t
    meta ! charset "utf-8"
    link ! href "//v4-alpha.getbootstrap.com/dist/css/bootstrap.min.css" ! rel "stylesheet"
  body b