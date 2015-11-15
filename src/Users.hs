{-# LANGUAGE TemplateHaskell #-}

module Users where

import qualified Web.Users.Persistent as WU
import qualified Web.Users.Types as WU
import           Control.Monad
import Data.Text (Text())
import           Database.Persist
import           Database.Persist.Postgresql
import Data.Int
import           Control.Monad.Reader
import Data.Time
import Language.Haskell.TH

import Users.TH
import Config
import Models (QLUser(), UserDetails)

backend :: AppM WU.Persistent
backend = do
  pool <- asks getPool
  return $ WU.Persistent (`runSqlPool` pool)

$(deriveReader 'backend)

initUserBackend' :: AppM ()
initUserBackend' = backend >>= \b -> liftIO (WU.initUserBackend b)

getUserIdByName :: Text -> AppM (Maybe WU.LoginId)
getUserById :: WU.LoginId -> AppM (Maybe QLUser)
listUsers :: Maybe (Int64, Int64) -> AppM [(WU.LoginId, QLUser)]
countUsers :: AppM Int64
createUser :: QLUser -> AppM (Either WU.CreateUserError WU.LoginId)
updateUser :: WU.LoginId -> (QLUser -> QLUser) -> AppM (Either WU.UpdateUserError ())
updateUserDetails :: WU.LoginId -> (UserDetails -> UserDetails) -> AppM ()
