{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Control.Monad.Logger        (runStderrLoggingT)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                   (toLower)
import           Data.Text                   (Text ())
import           Data.Time
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Web.Users.Persistent
import           Web.Users.Types

import           Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Liftsession json
    text Text
    date UTCTime
    user LoginId
    deriving Show

Profile json
    user LoginId
    deriving Show
|]

data Registration
    = Registration
    { regName         :: Text
    , regEmail        :: Text
    , regPassword     :: Text
    , regConfirmation :: Text
    } deriving (Eq, Show)

deriveJSON defaultOptions { fieldLabelModifier = map toLower . Prelude.drop 3, constructorTagModifier = map toLower } ''Registration

data Auth
    = Auth
    { authEmail    :: Text
    , authPassword :: Text
    } deriving (Eq, Show)

deriveJSON defaultOptions { fieldLabelModifier = map toLower . Prelude.drop 4, constructorTagModifier = map toLower } ''Auth

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b
runDb query = asks getPool >>= liftIO . runSqlPool query

db :: (MonadIO m, MonadBaseControl IO m) => SqlPersistM a -> m a
db query =
    runStderrLoggingT .
        withPostgresqlPool (connStr Development) 1 $
            liftIO . runSqlPersistMPool query


data Person = Person
    { name     :: Text
    , email    :: Text
    , personId :: LoginId
    } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

type QLUser = User UserDetails
type UserDetails = ()

userToPerson :: LoginId -> QLUser -> Person
userToPerson lid User {..} =
    Person { name = u_name
           , email = u_email
           , personId = lid
           }

convertRegistration :: Registration -> QLUser
convertRegistration Registration{..} =
    User { u_name = regName
         , u_email = regEmail
         , u_password = makePassword . PasswordPlain $ regPassword
         , u_more = ()
         , u_active = True
         }


data AuthResponse
    = AuthResponse
    { sessionId :: SessionId
    , person    :: Person
    } deriving (Eq, Show, Generic)

instance ToJSON AuthResponse
