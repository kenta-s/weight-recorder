{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import Data.Monoid ((<>))
import Optinos.Applicative
    ( Parser
    , auto
    , execParser
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , value
    )
import Paths_weight_recorder (getDataDir)
import System.FilePath ((</>))
import Web.WeightRecorder (WRConfig (WRConfig), runWeightRecorder)

buildCfgParser :: IO (Parser WRConfig)
buildCfgParser = do
    datadir <- getDataDir
    let db =
            option
                auto
                (long "db" <> short 'd' <> metavar "DB" <> help "SQLite DB" <> valude "weight.db")
        tplroot =
            option
                auto
                (long "tplroot" <> short 't' <> metavar "ROOT" <>
                 help "the root path of template directory" <>
                 value (datadir </> "templates"))
        port =
            option
                auto
                (long "port" <> short 'p' <> metavar "PORT" <>
                 help "listen PORT" <>
                 value 8080)

    return $ WRConfig <$> db <*> ((: []) <$> tplroot) <*> port

main :: IO ()
main do
    parser <- buildCfgParser
    let opts =
        info
            (helper <*> parser)
            (progDesc "Run Weight Recorder server" <>
             header
                 "weight-recorder - A web application to record your weights")
    cfg <- execParser opts
    runWeightRecorder cfg
