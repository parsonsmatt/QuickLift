{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Debug.Trace
import           Models
import           Network.Wai
import           Servant
import           Users
import qualified Web.Users.Persistent        as WU
import qualified Web.Users.Types             as WU

type QuickLiftAPI
    = "users" :> UserAPI
    :<|> "lifters" :> LifterAPI

type UserAPI = Get '[JSON] [Person]
    :<|> ReqBody '[JSON] Registration :> Post '[JSON] (Either Text.Text Int64)
    :<|> "login" :> ReqBody '[JSON] Auth :> Post '[JSON] (Maybe AuthResponse)
    :<|> "verify" :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe AuthResponse)

type LifterAPI = Get '[JSON] [Person]
    :<|> Capture "name" Text :> (Get '[JSON] Person
                            :<|> "sessions" :> SessionAPI)

type SessionAPI = Get '[JSON] [Entity Liftsession]
    :<|> Header "auth" Text :> ReqBody '[JSON] Liftsession :> Post '[JSON] (Either Text Int64)

userServer :: ServerT UserAPI AppM
userServer = getUsers :<|> registerUser :<|> authenticateUser
    :<|> verifyToken

lifterServer :: ServerT LifterAPI AppM
lifterServer = getUsers :<|> (\t -> getUser t :<|> sessionServer t)

verifyToken :: Text -> AppM (Maybe AuthResponse)
verifyToken sid = runMaybeT $ do
    let session = WU.SessionId sid
    userId <- MaybeT $ verifySession session 12000
    user <- MaybeT $ getUserById userId
    return (AuthResponse session (userToPerson userId user))

sessionServer :: Text -> ServerT SessionAPI AppM
sessionServer username = getSessions' :<|> createSession'
    where
        getSessions' :: AppM [Entity Liftsession]
        getSessions' = getUser username >>= getSessions

        createSession' :: Maybe Text -> Liftsession -> AppM (Either Text Int64)
        createSession' Nothing _ = lift $ left err401
        createSession' (Just sid) s = do
            loginId <- verifySession (WU.SessionId sid) 10
            user <- getUser username
            if loginId == Just (personId user)
               then createSession s user
               else lift $ left err401

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
server = userServer :<|> lifterServer

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
