{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Scotty
import Data.Monoid ((<>))

import Views(indexV)
import Routes(sessionsRoutes)

app :: ScottyM ()
app = do
    get     "/"             indexH
    get     "/about"        aboutH
    get     "/contact"      contactH
    get     "/login"        loginH
    get     "/register"     registerH
    sessionsRoutes
    get     "/:word"        wordH


indexH :: ActionM ()
indexH = indexV


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

