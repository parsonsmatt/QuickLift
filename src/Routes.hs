{-# LANGUAGE OverloadedStrings #-}

module Routes(
      routes
    ) where

import Config
import Routes.Home
import Routes.Sessions

routes :: App
routes = do
    homeRoutes
    sessionsRoutes


