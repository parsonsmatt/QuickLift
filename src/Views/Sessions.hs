{-# LANGUAGE OverloadedStrings #-}

module Views.Sessions where

import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A

import Config
import Views
import Views.Layout

indexSessionsV :: Action
indexSessionsV = blaze $ templateL $ p "List of your sessions"
