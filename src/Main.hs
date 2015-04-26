{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Web.Scotty

import App

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    app
