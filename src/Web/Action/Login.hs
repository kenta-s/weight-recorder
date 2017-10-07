{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Login (loginAction) where

import Control.Monad (when)
import Model.User (NewUser (NewUser), insertUser)
import Web.Core (WRAction, runSqlite)
import Web.Spock (param')
import Web.View.Start (startView)

loginAction :: WRAction a
loginAction = do
    name <- param' "name"
    password <- param' "password"
    when (null name || null password) $ startView (Just "name and password must be set")
    mUser <- runSqlite $ selectUser name password
    case mUser of
        Nothing -> StartView (Just "Failed to login")
        Just user -> do
            modifySession $
                \ses ->
                    ses
                    { wrsesUser = user
                    }
            redirect "/"
