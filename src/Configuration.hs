{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration
    (
    ) where

import Control.Monad.Except ( ExceptT
                            , MonadError
                            )
import Control.Monad.Reader ( MonadIO
                            , MonadReader
                            , ReaderT
                            )
import Database.Persist.Sql ( ConnectionPool )
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
