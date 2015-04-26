{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Scotty
import Data.Monoid ((<>))

import Views(indexV)

app :: ScottyM ()
app = do
    get     "/"         indexH
    get     "/:word"    wordH


indexH :: ActionM ()
indexH = indexV


wordH :: ActionM ()
wordH = do
    beam <- param "word"
    html $ "uh you said " <> beam <> "!"
