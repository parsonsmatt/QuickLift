{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static

import Web.Scotty

import App

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    app
