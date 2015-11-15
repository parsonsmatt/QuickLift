{-# LANGUAGE TemplateHaskell #-}

module Users 
  ( WU.LoginId
  , WU.CreateUserError(..)
  , WU.UpdateUserError(..)
  , initUserBackend
  , destroyUserBackend
  , housekeepBackend
  , getUserIdByName
  , getUserById
  , functionLevels
  ) where

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

initUserBackend :: AppM ()
initUserBackend = backend >>= liftIO . WU.initUserBackend

destroyUserBackend :: AppM ()
destroyUserBackend = backend >>= liftIO . WU.destroyUserBackend

housekeepBackend :: AppM ()
housekeepBackend = backend >>= liftIO . WU.housekeepBackend

getUserIdByName :: Text -> AppM (Maybe WU.LoginId)
getUserIdByName name = do
  b <- backend
  liftIO (WU.getUserIdByName b name)

getUserById :: WU.LoginId -> AppM (Maybe QLUser)
getUserById id = do
  b <- backend
  liftIO (WU.getUserById b id)

listUsers :: Maybe (Int64, Int64) -> AppM [(WU.LoginId, QLUser)]
listUsers range = do
  b <- backend
  liftIO (WU.listUsers b range)

countUsers :: AppM Int64
countUsers = backend >>= liftIO . WU.countUsers

createUser :: QLUser -> AppM (Either WU.CreateUserError WU.LoginId)
createUser u = do
  b <- backend 
  liftIO (WU.createUser b u)

updateUser :: WU.LoginId -> (QLUser -> QLUser) -> AppM (Either WU.UpdateUserError ())
updateUser u f = do
  b <- backend
  liftIO (WU.updateUser b u f)

updateUserDetails :: WU.LoginId -> (UserDetails -> UserDetails) -> AppM ()
updateUserDetails u f = do
  b <- backend
  liftIO (WU.updateUserDetails b u f)

deleteUser :: WU.LoginId -> AppM ()
deleteUser u = do
  b <- backend
  liftIO (WU.deleteUser b u)

authUser :: Text -> WU.PasswordPlain -> NominalDiffTime -> AppM (Maybe WU.SessionId)
authUser email pw time = do
  b <- backend
  liftIO (WU.authUser b email pw time)

--authUser' = \e p t -> backend >>= \b -> liftIO $ WU.authUser b e p t

deriveReader :: Name -> Name -> Q Exp
deriveReader rd fn = do
  info <- reify fn
  arity <- maybe (reportError "Unable to get arity of name" >> return 0)
        (return . functionLevels) 
        (getType info)
  varNames <- replicateM arity (newName "arg")
  expr <- [| rd >>= \b -> liftIO $ fn b $(foldl1 AppE <$> mapM varE varNames) |]
  if arity > 1
     then [| const 2 |]
     else [| const 1 |]



{-
in a very dumb way, what I want to do is:

deriveFn WU.authUser
1. inspect authUser's type
2. drop the first function in the type
splice in:
deriveFn WU.authUser =
  \e p t -> backend >>= \b -> liftIO (WU.authUser e p t)

Notes --

prints out the string representation of a function, specifically:

> $(stringE . show =<< reify (mkName "W.authUser"))

ClassOpI 
  Web.Users.Types.authUser 
  (ForallT 
    [KindedTV b_1627464633 StarT] 
    [AppT (ConT Web.Users.Types.UserStorageBackend) (VarT b_1627464633)] 
    (AppT 
      (AppT 
        ArrowT 
        (VarT b_1627464633))
      (AppT 
        (AppT 
          ArrowT 
          (ConT Data.Text.Internal.Text))
        (AppT 
          (AppT 
            ArrowT 
            (ConT Web.Users.Types.PasswordPlain))
          (AppT 
            (AppT 
              ArrowT 
              (ConT Data.Time.Clock.UTC.NominalDiffTime))
            (AppT 
              (ConT GHC.Types.IO)
              (AppT 
                (ConT GHC.Base.Maybe)
                (ConT Web.Users.Types.SessionId)
              )
            )
          )
        )
      )
    )
  )
  Web.Users.Types.UserStorageBackend 
  (Fixity 9 InfixL)

-}
--withAuthUser :: Text -> WU.PasswordPlain -> (QLUser -> Bool) -> (WU.LoginId -> IO r) -> AppM (Maybe r)
--withAuthUser email pw p f = do
--  b <- backend
--  liftIO (WU.withAuthUser b email pw p f)
