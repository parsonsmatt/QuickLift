{-# LANGUAGE OverloadedStrings #-}

module Routes(
      routes
    ) where

import Web.Scotty
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Routes.Sessions(sessionsRoutes)

import Views(homeV)

routes :: ScottyM ()
routes = do
    get     "/"             homeH
    get     "/about"        aboutH
    get     "/contact"      contactH
    get     "/login"        loginH
    get     "/register"     registerH
    sessionsRoutes


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
