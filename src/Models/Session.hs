{-# LANGUAGE OverloadedStrings #-}

module Models.Session where

import Data.Text as T

data Session = Session { date :: Date
                       , text :: Text 
                       }

data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 }
            deriving (Show, Eq, Ord)

parseDate :: Text -> Maybe Date
parseDate t = case splitOn "-" t of
                   [y,m,d] -> Just (Date (read $ unpack y) (read $ unpack m) (read $ unpack d))
                   _ -> Nothing
              
