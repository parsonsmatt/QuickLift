{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

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
import           Servant

type QuickLiftAPI 
  =    "users" :> Get '[JSON] [Person]
  :<|> "users" :> Capture "id" Int64 :> Get '[JSON] Person
  :<|> "users" :> ReqBody '[JSON] Registration :> Post '[JSON] Int64
  :<|> "sessions" :> ReqBody '[JSON] Session :> Post '[JSON] Int64
  :<|> "users" :> Capture "id" Int64 :> "sessions" :> Get '[JSON] [Session]

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT QuickLiftAPI AppM
server = allPersons
  :<|> singlePerson
  :<|> createPerson
  :<|> createSession
  :<|> userSessions

userSessions :: Int64 -> AppM [Session]
userSessions uId =
  map entityVal <$> runDb (selectList [SessionUserId ==. toSqlKey uId] [])

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

createPerson :: Registration -> AppM Int64
createPerson p =
  liftM fromSqlKey (runDb $ insert $ User (Text.unpack $ regName p) (Text.unpack $ regEmail p))

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
