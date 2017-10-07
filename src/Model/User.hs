{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Model.User
    ( NewUser(NewUser, nuName, nuPassword)
    , nuName'
    , nuPassword'
    , insertUser
    , selectUser
    )
    where

import Control.Exception (catch)
import Crypto.BCrypt
    ( hashPasswordUsingPolicy
    , slowerBcryptHashingPolicy
    , validatePassword
    )
import qualified Data.ByteString as BS
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.HDBC
    ( IConnection
    , SqlError
    , withTransaction
    )
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import qualified Database.HDBC.Record as DHR
import qualified Database.HDBC.Relational.Query as HRR
import qualified Entity.User as User
import System.IO (hPrint, hPutStrLn, stderr)
