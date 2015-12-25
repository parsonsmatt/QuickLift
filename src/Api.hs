{-# LANGUAGE DataKinds     #-}
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
import Users

type QuickLiftAPI 
    =    "users" :> Get '[JSON] [Person]
    :<|> "sessions" :> ReqBody '[JSON] LiftSession :> Post '[JSON] Int64
    :<|> "users" :> Capture "id" Int64 :> "sessions" :> Get '[JSON] [Entity LiftSession]

server :: ServerT QuickLiftAPI AppM
server = allPersons
    :<|> createLiftSession
    :<|> userLiftSessions

userLiftSessions :: Int64 -> AppM [Entity LiftSession]
userLiftSessions uId =
  runDb (selectList [LiftSessionUserId ==. toSqlKey uId] [])

createLiftSession :: LiftSession -> AppM Int64
createLiftSession = liftM fromSqlKey . runDb . insert

allPersons :: AppM [Person]
allPersons = map (userToPerson . snd) <$> listUsers Nothing


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
