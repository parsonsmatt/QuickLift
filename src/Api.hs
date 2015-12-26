{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Debug.Trace
import           Config
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8       as BS
import           Data.Int
import qualified Data.Text                   as Text
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
    :<|> "login" :> ReqBody '[JSON] Auth :> Post '[JSON] (Maybe SessionId)

userServer :: ServerT UserAPI AppM
userServer = getUsers :<|> registerUser :<|> authenticateUser

getUsers :: AppM [Person]
getUsers = do
    users <- listUsers Nothing
    return (map (userToPerson . snd) users)

registerUser :: Registration -> AppM (Either Text.Text Int64)
registerUser reg = do
    let qlUser = convertRegistration reg 
    user <- createUser qlUser
    return $ either (Left . Text.pack . show) (Right . fromSqlKey) user

authenticateUser :: Auth -> AppM (Maybe SessionId)
authenticateUser auth =
    return Nothing


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
