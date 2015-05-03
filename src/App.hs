{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Scotty

import Models()
import Routes(routes)

app :: ScottyM ()
app = routes

