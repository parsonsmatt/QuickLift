{-# LANGUAGE OverloadedStrings #-}

module Views.Home where

-- import Web.Scotty()

import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A

import Views.Layout

homeV :: Html
homeV = do
    headL
    headerL
    footerL

