{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static

import Web.Scotty.Trans

import App

main :: IO ()
main = scottyT 3000 id id $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    app
