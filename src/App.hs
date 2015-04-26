{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Scotty
import Data.Monoid ((<>))

app :: ScottyM ()
app = do
    get     "/"         indexH
    get     "/:word"    wordH


indexH :: ActionM ()
indexH = html "Hello World"


wordH :: ActionM ()
wordH = do
    beam <- param "word"
    html $ "uh you said " <> beam <> "!"
