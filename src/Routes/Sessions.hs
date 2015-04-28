{-# LANGUAGE OverloadedStrings #-}

module Routes.Sessions where

import Data.Monoid ((<>))
import Web.Scotty

import Views()

sessionsRoutes :: ScottyM ()
sessionsRoutes = do
    get     "/sessions"          indexSessionsH
    post    "/sessions"          createSessionH
    get     "/sessions/new"      newSessionH
    get     "/sessions/:id"      showSessionH
    get     "/sessions/:id/edit" editSessionH
    patch   "/sessions/:id"      updateSessionH
    delete  "/sessions/:id"      deleteSessionH


indexSessionsH :: ActionM ()
indexSessionsH = html "List of your sessions!"


newSessionH :: ActionM ()
newSessionH = html "makin a new Session"


createSessionH :: ActionM ()
createSessionH = do
    session <- param "session"
    html $ "You posted a session: " <> session <> "!"


deleteSessionH :: ActionM ()
deleteSessionH = html "deletin it"


showSessionH :: ActionM ()
showSessionH = do
    sessionId <- param "id"
    html $ "Requesting Session " <> sessionId <> "!"


editSessionH :: ActionM ()
editSessionH = do
    sessionId <- param "id"
    html $ "Editing session " <> sessionId <> "!"


updateSessionH :: ActionM ()
updateSessionH = html "updatesss"
