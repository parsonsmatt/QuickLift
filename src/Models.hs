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

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Data.DateTime
import Data.Text as T

import Models.Session()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Session
    sessionText Text
    date DateTime
    deriving Show
|]
