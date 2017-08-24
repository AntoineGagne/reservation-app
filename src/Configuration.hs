{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration
    ( Application (..)
    , Configuration (..)
    , Environment (..)
    , environmentPool
    , makePool
    ) where

import Control.Monad.Logger ( runStdoutLoggingT )
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
    deriving (Eq, Read, Show)

makePool :: Environment -> IO ConnectionPool
makePool Development = runStdoutLoggingT $ createSqlitePool "sqlite.db" (environmentPool Development)
makePool Production = runStdoutLoggingT $ createSqlitePool "sqlite.db" (environmentPool Production)

environmentPool :: Environment -> Int
environmentPool Development = 1
environmentPool Production = 8
