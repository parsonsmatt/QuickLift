{-# LANGUAGE OverloadedStrings #-}

module Views.Home where

import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A

import Views.Layout

homeV :: Html
homeV = templateL $ p "Welcome to QuickLift"


aboutV :: Html
aboutV = templateL $ p "This is a weightlifting app!"


contactV :: Html
contactV = templateL $ p "Contact me at parsonsmatt@gmail.com"


loginV :: Html
loginV = templateL $ p "Login"


registerV :: Html
registerV = templateL $ p "Register"
