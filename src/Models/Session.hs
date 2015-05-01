{-# LANGUAGE OverloadedStrings #-}

module Models.Session where

import Data.Dates
import Data.Text as T

data Session = Session { date :: DateTime
                       , text :: Text 
                       }
