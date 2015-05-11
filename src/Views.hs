module Views where

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans
import Config

blaze :: Html -> Action
blaze = html . renderHtml

