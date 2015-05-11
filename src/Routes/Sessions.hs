{-# LANGUAGE OverloadedStrings #-}
module Routes.Sessions(
    sessionsRoutes
  ) where

import Data.Monoid ((<>))
import Data.Text.Lazy as T
import Web.Scotty.Trans

import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)

import qualified Database.Persist.Postgresql as DB

import Config
import Models
import Views
import Views.Sessions


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
indexSessionsH = blaze indexSessionsV


newSessionH :: Action
newSessionH = blaze newSessionV


createSessionH :: Action
createSessionH = do
    session <- param "session"
    time <- liftIO getCurrentTime
    sId <- runDb $ DB.insert $ Session session time
    let tsId = (T.pack . show) sId
    blaze (createSessionV session tsId)


deleteSessionH :: Action
deleteSessionH = do
    session <- param "id"
    _ <- runDb $ DB.delete (DB.toSqlKey $ read session :: DB.Key Session)
    blaze (deleteSessionV (T.pack session))


showSessionH :: Action
showSessionH = do
    sessionId <- param "id"
    session <- runDb $ DB.get $ DB.toSqlKey $ read sessionId
    blaze (showSessionV session)

editSessionH :: Action
editSessionH = do
    sessionId <- param "id"
    html $ "Editing session " <> sessionId <> "!"


updateSessionH :: Action
updateSessionH = html "updatesss"

-- data Date = Date { year :: Int
--                  , month :: Int
--                  , day :: Int
--                  }
--             deriving (Show, Eq, Ord)
-- 
-- parseDate :: Text -> Maybe Date
-- parseDate t = let toNum = read . unpack
--               in  case splitOn "-" t of
--                        [y,m,d] -> Just Date { year = toNum y, month = toNum m, day = toNum d } 
--                        _ -> Nothing
