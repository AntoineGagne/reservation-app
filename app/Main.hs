module Main where

import Database.Persist.Sqlite ( runSqlPool )
import Data.Maybe ( fromMaybe )
import Network.Wai.Handler.Warp ( run )
import Safe ( readMay )
import System.Environment ( lookupEnv )

import Lib ( app )
import Configuration ( Configuration (..)
                     , Environment (..)
                     , makePool
                     , setLogger
                     )
import Model ( doMigrations )

-- Taken mostly from
-- http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html
-- (Fri Aug 25 16:12:40 EDT 2017)
main :: IO ()
main = do
    environment <- lookupSetting "ENVIRONMENT" Development
    port <- lookupSetting "PORT" 8080
    pool <- makePool environment
    let configuration = Configuration { getPool = pool, getEnvironment = environment }
        logger = setLogger environment
    runSqlPool doMigrations pool
    run port $ logger $ app configuration


lookupSetting :: Read a => String -> a -> IO a
lookupSetting environment defaultValue = do
    maybeValue <- lookupEnv environment
    case maybeValue of
        Nothing -> return defaultValue
        Just value -> maybe (handleFailedRead value) return (readMay value)
    where
        handleFailedRead value' =
            error $ mconcat [ "Failed to read [("
                            , value'
                            , ")] for environment variable "
                            , environment
                            ]
