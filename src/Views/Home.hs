{-# LANGUAGE OverloadedStrings #-}

module Views.Home where

import Web.Scotty


homeV :: ActionM ()
homeV = html "HelloWorld"
