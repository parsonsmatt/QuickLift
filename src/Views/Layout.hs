{-# LANGUAGE OverloadedStrings #-}

module Views.Layout(
      headL
    , headerL
    , footerL
  ) where

import Web.Scotty ()

import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes

headL :: Html
headL = docTypeHtml $ 
    H.head $ 
        H.title "QuickLift"
    
headerL :: Html
headerL = 
    header $ do
        h1 "QuickLift"
        H.p $ do
            H.a ! href "register" $ "Register" 
            H.a ! href "login" $ "Login"
            H.a ! href "sessions" $ "Sessions"


footerL :: Html
footerL = undefined
