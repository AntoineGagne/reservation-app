{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration
    ( Application (..)
    , Configuration (..)
    , Environment (..)
    , environmentPool
    , makePool
    , setLogger
    ) where

import Control.Monad.Logger ( runStdoutLoggingT
                            , runNoLoggingT
                            )
import Control.Monad.Except ( ExceptT
                            , MonadError
                            , liftIO
                            )
import Control.Monad.Reader ( MonadIO
                            , MonadReader
                            , ReaderT
                            )
import Database.Persist.Sql ( ConnectionPool )
import Database.Persist.Sqlite ( createSqlitePool )
import Network.Wai ( Middleware )
import Network.Wai.Middleware.RequestLogger ( logStdout
                                            , logStdoutDev
                                            )
import Servant ( ServantErr )


newtype Application a
    = Application
        { runApp :: ReaderT Configuration (ExceptT ServantErr IO) a
        } deriving ( Applicative
                   , Functor
                   , Monad
                   , MonadReader Configuration
                   , MonadError ServantErr
                   , MonadIO
                   )

data Configuration
    = Configuration
        { getPool :: ConnectionPool
        , getEnvironment :: Environment
        }

data Environment
    = Development
    | Production
    | Test
    deriving (Eq, Read, Show)

setLogger :: Environment -> Middleware
setLogger Development = logStdoutDev
setLogger Production = logStdout
setLogger Test = id

makePool :: Environment -> IO ConnectionPool
makePool Development = runStdoutLoggingT $ createSqlitePool "sqlite.db" (environmentPool Development)
makePool Production = runStdoutLoggingT $ createSqlitePool "sqlite.db" (environmentPool Production)
makePool Test = runNoLoggingT $ createSqlitePool "sqlite-test.db" (environmentPool Test)

environmentPool :: Environment -> Int
environmentPool Development = 1
environmentPool Production = 8
environmentPool Test = 1
