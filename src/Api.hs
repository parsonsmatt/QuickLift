{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.Wai
import Database.Persist.Postgresql
import Control.Monad
import Data.Int
import Servant
import Config
import Models

type QuickLiftAPI = 
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person
    :<|> "users" :> ReqBody '[JSON] Person :> Post '[JSON] Int64
    :<|> "sessions" :> ReqBody '[JSON] Session :> Post '[JSON] Int64
    :<|> "users" :> Capture "id" Int64 :> "sessions" :> Get '[JSON] [Entity Session]

type AppM = ReaderT Config (EitherT ServantErr IO)

userAPI :: Proxy QuickLiftAPI
userAPI = Proxy

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

readerServer :: Config -> Server QuickLiftAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT QuickLiftAPI AppM
server = allPersons :<|> singlePerson :<|> createPerson :<|> createSession :<|> userSessions

userSessions :: Int64 -> AppM [Entity Session]
userSessions uId = runDb (selectList [SessionUserId ==. toSqlKey uId] [])

createSession :: Session -> AppM Int64
createSession session = liftM fromSqlKey $ runDb $ insert session

allPersons :: AppM [Person]
allPersons = do
    users <- runDb $ selectList [] []
    let people = map (\(Entity _ y) -> userToPerson y) users
    return people

singlePerson :: String -> AppM Person
singlePerson str = do
    users <- runDb $ selectList [UserName ==. str] []
    let list = map (\(Entity _ y) -> userToPerson y) users
    case list of
         []     -> lift $ left err404
         (x:xs) -> return x

createPerson :: Person -> AppM Int64
createPerson p = do
    newPerson <- runDb $ insert $ User (name p) (email p)
    return $ fromSqlKey newPerson
