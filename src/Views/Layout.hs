{-# LANGUAGE OverloadedStrings #-}

module Views.Layout(
      headL
    , headerL
    , footerL
  ) where

import Web.Scotty ()

import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

headL :: Html
headL = docTypeHtml $ 
    H.head $ do
        H.title "QuickLift"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/css/ql.css"
        script ! src "/js/ql.js" $ ""
        meta ! A.name "viewport" ! content "width=device-width"
    
headerL :: Html
headerL = 
    header $ do
        h1 "QuickLift"
        ul $ do
            li $
                a ! href "/register" $ "Register" 
            li $
                a ! href "/login" $ "Login"
            li $
                a ! href "/sessions" $ "Sessions"


footerL :: Html
footerL = footer "am a footer"
