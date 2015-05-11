{-# LANGUAGE OverloadedStrings #-}

module Routes.Home
    ( homeRoutes
    ) where

import Web.Scotty.Trans

import Views
import Views.Home
import Config

homeRoutes :: App
homeRoutes = do
    get     "/"             homeH
    get     "/about"        aboutH
    get     "/contact"      contactH
    get     "/login"        loginH
    get     "/register"     registerH

homeH :: Action
homeH = blaze homeV 

aboutH :: Action
aboutH = blaze aboutV

contactH :: Action
contactH = blaze contactV

loginH :: Action
loginH = blaze loginV

registerH :: Action
registerH = blaze registerV
