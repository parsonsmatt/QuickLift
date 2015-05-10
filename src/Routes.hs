{-# LANGUAGE OverloadedStrings #-}

module Routes(
      routes
    ) where

import Web.Scotty.Trans
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Config
import Routes.Sessions(sessionsRoutes)
import Views(homeV)

routes :: App
routes = do
    get     "/"             homeH
    get     "/about"        aboutH
    get     "/contact"      contactH
    get     "/login"        loginH
    get     "/register"     registerH
    sessionsRoutes


blaze :: Html -> Action
blaze = html . renderHtml

homeH :: Action
homeH = blaze homeV 

aboutH :: Action
aboutH = html "This is a weightlifting app!"

contactH :: Action
contactH = html "Contact me at parsonsmatt@gmail.com!"

loginH :: Action
loginH = html "Login!"

registerH :: Action
registerH = html "Register!"
