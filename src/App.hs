{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Scotty
import Data.Monoid ((<>))

import Views(indexV)

app :: ScottyM ()
app = do
    get     "/"             indexH
    get     "/about"        aboutH
    get     "/contact"      contactH
    get     "/login"        loginH
    get     "/register"     registerH
    get     "/sessions"     sessionsH
    post    "/sessions"     postSessionsH
    get     "/sessions/:id" getSessionH
    get     "/sessions/:id/edit" editSessionH
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


sessionsH :: ActionM ()
sessionsH = html "List of your sessions!"


postSessionsH :: ActionM ()
postSessionsH = do
    session <- param "session"
    html $ "You posted a session: " <> session <> "!"


getSessionH :: ActionM ()
getSessionH = do
    sessionId <- param "id"
    html $ "Requesting Session " <> sessionId <> "!"


editSessionH :: ActionM ()
editSessionH = do
    sessionId <- param "id"
    html $ "Editing session " <> sessionId <> "!"
    
wordH :: ActionM ()
wordH = do
    beam <- param "word"
    html $ "uh you said " <> beam <> "!"

