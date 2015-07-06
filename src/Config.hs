{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad.Logger
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger

import           Database.Persist.Postgresql

data Config = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv  = Development
    }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createPostgresqlPool (connStr Test) (envPool Test)
makePool e = runStdoutLoggingT $ createPostgresqlPool (connStr e) (envPool e)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=quicklift user=test password=test port=5432"
