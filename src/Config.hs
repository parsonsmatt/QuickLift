{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (runStdoutLoggingT)

import Web.Scotty.Trans
import Data.Text.Lazy

import Database.Persist.Postgresql


data Config = Config { getPool :: ConnectionPool }

newtype ConfigM a = ConfigM
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad,
                MonadIO, MonadReader Config)

type Error = Text
type Action = ActionT Error ConfigM ()
type App = ScottyT Error ConfigM ()


makePool :: IO ConnectionPool
makePool = runStdoutLoggingT $ createPostgresqlPool connStr 10


connStr :: ConnectionString
connStr = "host=localhost dbname=QuickLift user=test password=test port=5432"
