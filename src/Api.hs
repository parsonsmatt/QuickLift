{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.Wai
import Database.Persist.Postgresql
import Database.Persist
import Control.Monad
import Data.Int
import Servant
import Config
import Models

type QuickLiftAPI = 
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "id" Int64 :> Get '[JSON] Person
    :<|> "users" :> ReqBody '[JSON] Person :> Post '[JSON] Int64
    :<|> "sessions" :> ReqBody '[JSON] Session :> Post '[JSON] Int64
    :<|> "users" :> Capture "id" Int64 :> "sessions" :> Get '[JSON] [Entity Session]

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT QuickLiftAPI AppM
server = allPersons 
    :<|> singlePerson 
    :<|> createPerson 
    :<|> createSession 
    :<|> userSessions

userSessions :: Int64 -> AppM [Entity Session]
userSessions uId = runDb (selectList [SessionUserId ==. toSqlKey uId] [])

createSession :: Session -> AppM Int64
createSession = liftM fromSqlKey . runDb . insert

allPersons :: AppM [Person]
allPersons = liftM (map (\(Entity _ y) -> userToPerson y)) 
                   (runDb $ selectList [] []) 

singlePerson :: Int64 -> AppM Person
singlePerson uId = do
    user <- runDb $ get (toSqlKey uId)
    case userToPerson <$> user of
         Nothing -> lift $ left err404
         Just x  -> return x

createPerson :: Person -> AppM Int64
createPerson p = liftM fromSqlKey (runDb $ insert $ User (name p) (email p))



userAPI :: Proxy QuickLiftAPI
userAPI = Proxy

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

readerServer :: Config -> Server QuickLiftAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

