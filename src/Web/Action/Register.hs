{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Register (registerAction) where

import Control.Monad (when)
import Model.User (NewUser (NewUser), insertUser)
import Web.Core (WRAction, runSqlite)
import Web.Spock (param')
import Web.View.Start (startView)

registerAction :: WRAction a
registerAction = do
    name <- param' "name"
    password <- param' "password"
    when (null name || null password) $ startView (Just "name and password must be set")
    n <- runSqlite $ insertUser (NewUser name password)
    when (n <= 0) $ startView (Just "Failed to register")
    startView (Just "Registration complete")
