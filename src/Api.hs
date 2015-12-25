{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

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
import           Users

type QuickLiftAPI
    = "users" :> UserAPI

type UserAPI = Get '[JSON] [Person]
    :<|> ReqBody '[JSON] Registration :> Post '[JSON] Int64

userServer :: ServerT UserAPI AppM
userServer = getUsers :<|> registerUser

getUsers :: AppM [Person]
getUsers = do
    users <- listUsers Nothing
    return (map (userToPerson . snd) users)

registerUser :: Registration -> AppM Int64
registerUser reg = do
    let qlUser = convertRegistration reg 
    user <- createUser qlUser
    case user of
         Left _ -> lift $ left err403
         Right id -> return 0

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
