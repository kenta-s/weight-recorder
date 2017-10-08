{-# LANGUAGE OverloadedStrings #-}

module Web.Action.Login (loginAction) where

import Control.Monad (when)
import Model.User (selectUser)
import Web.Core (WRAction, runSqlite, wrsesUser)
import Web.Spock (modifySession, param', redirect)
import Web.View.Start (startView)

loginAction :: WRAction a
loginAction = do
    name <- param' "name"
    password <- param' "password"
    when (null name || null password) $ startView (Just "name and password must be set")
    mUser <- runSqlite $ selectUser name password
    case mUser of
        Nothing -> startView (Just "Failed to login")
        Just user -> do
            modifySession $
                \ses ->
                    ses
                    { wrsesUser = Just user
                    }
            redirect "/"
