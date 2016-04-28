{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Environment                   (lookupEnv)

import           Database.Persist.Postgresql

data Config
    = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

type AppM = ReaderT Config (ExceptT ServantErr IO)

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig
    = Config
    { getPool = undefined
    , getEnv  = Development
    }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createPostgresqlPool (connStr Test) (envPool Test)
makePool Development = runStdoutLoggingT $ createPostgresqlPool (connStr Development) (envPool Development)
makePool Production = do
    pool <- runMaybeT $ do
        let keys = fmap BS.pack
                   [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        prodStr <- mconcat . zipWith (<>) keys . fmap BS.pack
                  <$> traverse (MaybeT . lookupEnv) envs
        runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
    case pool of
         Nothing -> error "Database Configuration not present in environment."
         Just a -> return a

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=quicklift user=test password=test port=5432"
