{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Users where

import           Control.Monad               ()
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int
import           Data.Text                   (Text ())
import           Data.Time
import           Database.Persist
import           Database.Persist.Postgresql
import           GHC.Generics
import           Language.Haskell.TH
import qualified Web.Users.Persistent        as WU
import qualified Web.Users.Types             as WU

import           Config
import           Models                      (QLUser (), UserDetails)
import           Users.TH

backend :: AppM WU.Persistent
backend = do
  pool <- asks getPool
  return $ WU.Persistent (`runSqlPool` pool)


type SessionId = WU.SessionId

data RegistrationError
    = EmailAlreadyTaken
    deriving (Show, Generic)

deriveJSON defaultOptions ''RegistrationError

deriveReader 'backend

getUserIdByName :: Text -> AppM (Maybe WU.LoginId)
getUserById :: WU.LoginId -> AppM (Maybe QLUser)
listUsers :: Maybe (Int64, Int64) -> AppM [(WU.LoginId, QLUser)]
countUsers :: AppM Int64
createUser :: QLUser -> AppM (Either WU.CreateUserError WU.LoginId)
updateUser :: WU.LoginId -> (QLUser -> QLUser) -> AppM (Either WU.UpdateUserError ())
updateUserDetails :: WU.LoginId -> (UserDetails -> UserDetails) -> AppM ()
