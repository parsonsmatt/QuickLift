module Server where

import           Control.Monad               (liftM)
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           Web.Users.Types             (initUserBackend)
import           Web.Users.Persistent        (Persistent(..))

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              defaultConfig, makePool,
                                              setLogger)
import           Models                      (doMigrations)
import Util


quickLift :: IO ()
quickLift = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    initUserBackend (Persistent (`runSqlPool` pool))
    run port $ logger $ app cfg
