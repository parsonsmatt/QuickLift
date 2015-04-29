{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Scotty
import Data.Monoid ((<>))
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Views(homeV)
import Routes(sessionsRoutes)

app :: ScottyM ()
app = do
    get     "/"             homeH
    get     "/about"        aboutH
    get     "/contact"      contactH
    get     "/login"        loginH
    get     "/register"     registerH
    sessionsRoutes
    get     "/:word"        wordH


blaze :: Html -> ActionM ()
blaze = html . renderHtml

homeH :: ActionM ()
homeH = blaze homeV 


aboutH :: ActionM ()
aboutH = html "This is a weightlifting app!"


contactH :: ActionM ()
contactH = html "Contact me at parsonsmatt@gmail.com!"


loginH :: ActionM ()
loginH = html "Login!"


registerH :: ActionM ()
registerH = html "Register!"


wordH :: ActionM ()
wordH = do
    beam <- param "word"
    html $ "uh you said " <> beam <> "!"

