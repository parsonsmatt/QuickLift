{-# LANGUAGE OverloadedStrings #-}

module Routes.Sessions(
    sessionsRoutes
  ) where

import Data.Monoid ((<>))
import Data.Text.Lazy as T
import Web.Scotty.Trans

import Views()
import Config (App, Action)

sessionsRoutes :: App
sessionsRoutes = do
    get     "/sessions"          indexSessionsH
    post    "/sessions"          createSessionH
    get     "/sessions/new"      newSessionH
    get     "/sessions/:id"      showSessionH
    get     "/sessions/:id/edit" editSessionH
    patch   "/sessions/:id"      updateSessionH
    delete  "/sessions/:id"      deleteSessionH


indexSessionsH :: Action
indexSessionsH = html "List of your sessions!"


newSessionH :: Action
newSessionH = html "makin a new Session"


createSessionH :: Action
createSessionH = do
    session <- param "session"
    html $ "You posted a session: " <> session <> "!"


deleteSessionH :: Action
deleteSessionH = html "deletin it"


showSessionH :: Action
showSessionH = do
    sessionId <- param "id"
    case parseDate sessionId of
         Just d  -> html $ "Requestion Session from date " <> T.pack (show d)
         Nothing -> html "Uh what?"


editSessionH :: Action
editSessionH = do
    sessionId <- param "id"
    html $ "Editing session " <> sessionId <> "!"


updateSessionH :: Action
updateSessionH = html "updatesss"

data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 }
            deriving (Show, Eq, Ord)

parseDate :: Text -> Maybe Date
parseDate t = case splitOn "-" t of
                   [y,m,d] -> Just (Date (read $ unpack y) (read $ unpack m) (read $ unpack d))
                   _ -> Nothing
              
