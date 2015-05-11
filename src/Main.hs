{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Control.Monad.Trans.Reader(runReaderT)

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static

import Web.Scotty.Trans

import Routes
import Models
import Config

main :: IO ()
main = do
    pool <- makePool
    let config = Config { getPool = pool }
        r m = runReaderT (runConfigM m) config
    scottyT 3000 r r $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "static")
        runDb doMigrations
        routes
