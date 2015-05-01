{-# LANGUAGE OverloadedStrings #-}

module Routes.Sessions(
    sessionsRoutes
  ) where

import Data.Monoid ((<>))
import Data.Text.Lazy as T
import Web.Scotty

import Views()
import Models.Session

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
    case parseDate sessionId of
         Just d  -> html $ "Requestion Session from date " <> T.pack (show d)
         Nothing -> html "Uh what?"


editSessionH :: ActionM ()
editSessionH = do
    sessionId <- param "id"
    html $ "Editing session " <> sessionId <> "!"


updateSessionH :: ActionM ()
updateSessionH = html "updatesss"
