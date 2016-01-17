module Util where

import           System.Environment          (lookupEnv)

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = maybe def read <$> lookupEnv env
