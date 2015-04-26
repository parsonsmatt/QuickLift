{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import Web.Scotty


indexV :: ActionM ()
indexV = html "HelloWorld"
