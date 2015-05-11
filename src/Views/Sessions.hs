{-# LANGUAGE OverloadedStrings #-}

module Views.Sessions where

import Data.Monoid ((<>))

import Data.Text.Lazy
import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A

import Models
import Views.Layout

indexSessionsV :: Html
indexSessionsV = templateL $ p "List of your sessions"


newSessionV :: Html
newSessionV = templateL $ p "Making a new session"


createSessionV :: Text -> Text -> Html
createSessionV session sId = templateL $ do
        p "You posted a session: " <> toHtml session <> "!"
        p "The ID of that session is: " <> toHtml sId


deleteSessionV :: Text -> Html
deleteSessionV sId = templateL $ p "Deleted session " <> toHtml sId


showSessionV :: Maybe Session -> Html
showSessionV session = case session of
                Just sesh -> "Requested session: <br>" <> toHtml (show sesh) 
                Nothing -> "Session not found"


