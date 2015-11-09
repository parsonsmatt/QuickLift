{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Crypto.PasswordStore
import           Config
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Int
import           Database.Persist
import           Database.Persist.Postgresql
import           Models
import           Network.Wai
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Servant
import qualified Data.ByteString.Char8 as BS

type QuickLiftAPI 
  =    "users" :> Get '[JSON] [Person]
  :<|> "users" :> Capture "id" Int64 :> Get '[JSON] Person
  :<|> "users" :> ReqBody '[JSON] Registration :> Post '[JSON] Int64
  :<|> "sessions" :> ReqBody '[JSON] Session :> Post '[JSON] Int64
  :<|> "users" :> Capture "id" Int64 :> "sessions" :> Get '[JSON] [Entity Session]
  :<|> "authentication" :> ReqBody '[JSON] Auth :> Post '[JSON] (Maybe (Entity User))

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT QuickLiftAPI AppM
server = allPersons
  :<|> singlePerson
  :<|> createPerson
  :<|> createSession
  :<|> userSessions
  :<|> handleAuth

userSessions :: Int64 -> AppM [Entity Session]
userSessions uId =
  runDb (selectList [SessionUserId ==. toSqlKey uId] [])

createSession :: Session -> AppM Int64
createSession = liftM fromSqlKey . runDb . insert

allPersons :: AppM [Person]
allPersons = map (userToPerson . entityVal) <$> runDb (selectList [] [])

singlePerson :: Int64 -> AppM Person
singlePerson uId = do
    user <- runDb $ get (toSqlKey uId)
    case userToPerson <$> user of
         Nothing -> lift $ left err404
         Just x  -> return x

handleAuth :: Auth -> AppM (Maybe (Entity User))
handleAuth Auth{..} = do
  muser <- runDb . getBy . UniqueEmail . Text.unpack $ authEmail
  let authed = do
          user <- muser
          let attempt = Text.encodeUtf8 authPassword
              actual  = Text.encodeUtf8 . userPasswordHash . entityVal $ user
          guard $ verifyPassword attempt actual 
          return user
  case authed of
       Nothing -> lift $ left err401
       Just u -> return (Just u)


createPerson :: Registration -> AppM Int64
createPerson Registration{..} = do
  pw <- Text.decodeUtf8 <$> liftIO (makePassword (Text.encodeUtf8 regPassword) 17)
  i <- runDb $ insert $ User (Text.unpack regName) (Text.unpack regEmail) pw
  return (fromSqlKey i)

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
