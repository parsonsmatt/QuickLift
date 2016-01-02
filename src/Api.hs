{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Config
import           Control.Monad
import           Control.Monad.Reader
import Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Either
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8       as BS
import           Data.Int
import Data.Maybe
import qualified Data.Text                   as Text
import Data.Text (Text)
import qualified Data.Text.Encoding          as Text
import           Database.Persist
import           Database.Persist.Postgresql
import           Models
import           Network.Wai
import           Servant
import qualified Web.Users.Types as WU
import qualified Web.Users.Persistent as WU
import           Users

type QuickLiftAPI
    = "users" :> UserAPI

type UserAPI = Get '[JSON] [Person]
    :<|> ReqBody '[JSON] Registration :> Post '[JSON] (Either Text.Text Int64)
    :<|> "login" :> ReqBody '[JSON] Auth :> Post '[JSON] (Maybe AuthResponse)
    :<|> Capture "name" Text :> (Get '[JSON] Person
                            :<|> "sessions" :> SessionAPI)

type SessionAPI = Get '[JSON] [Entity Liftsession]
    :<|> ReqBody '[JSON] CreateSession :> Post '[JSON] (Either Text Int64)

userServer :: ServerT UserAPI AppM
userServer = getUsers :<|> registerUser :<|> authenticateUser :<|> (\t -> getUser t
    :<|> sessionServer t)

sessionServer :: Text -> ServerT SessionAPI AppM
sessionServer username = getSessions' :<|> createSession'
    where
        getSessions' :: AppM [Entity Liftsession]
        getSessions' = getUser username >>= getSessions

        createSession' :: Liftsession -> AppM (Either Text Int64)
        createSession' s = getUser username >>= createSession s

getSessions :: Person -> AppM [Entity Liftsession]
getSessions Person {..} =
    runDb $ selectList [ LiftsessionUser ==. personId ] []

createSession :: Liftsession -> Person -> AppM (Either Text Int64)
createSession ls person = do
    let ls' = ls { liftsessionUser = personId person }
    key <- runDb $ insert ls'
    return . return . fromSqlKey $ key

getUsers :: AppM [Person]
getUsers = do
    users <- listUsers Nothing
    return (map (uncurry userToPerson) users)

getUser :: Text -> AppM Person
getUser k = do
    person <- runMaybeT $ do
        userid <- MaybeT $ getUserIdByName k
        user <- MaybeT $ getUserById userid
        return $ userToPerson userid user
    case person of
         Nothing -> lift $ left err404
         Just person -> return person

registerUser :: Registration -> AppM (Either Text.Text Int64)
registerUser reg = do
    let qlUser = convertRegistration reg
    user <- createUser qlUser
    return $ either (Left . Text.pack . show) (Right . fromSqlKey) user

authenticateUser :: Auth -> AppM (Maybe AuthResponse)
authenticateUser auth = runMaybeT $ do
    sessionId <- MaybeT $ authUser (authEmail auth) (WU.PasswordPlain $ authPassword auth) 1200000
    person <- lift $ getUser (authEmail auth)
    return $ AuthResponse sessionId person



server :: ServerT QuickLiftAPI AppM
server = userServer

quickliftAPI :: Proxy QuickLiftAPI
quickliftAPI = Proxy

type AppAPI = QuickLiftAPI :<|> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

files :: Application
files = serveDirectory "ql-ui/assets/"

app :: Config -> Application
app cfg = serve appAPI (readerServer cfg :<|> files)

readerServer :: Config -> Server QuickLiftAPI
readerServer cfg = enter (runReaderTNat cfg) server
