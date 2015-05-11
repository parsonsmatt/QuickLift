{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Monad.Trans.Reader

import Data.Text.Lazy
import Data.Time.Clock

import Database.Persist.Postgresql 
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Session
    sessionText Text
    day UTCTime
    lifterId UserId
    deriving Show

User
    email String
    password String
    deriving Show
|]


doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll
