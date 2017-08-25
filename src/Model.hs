{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model
    ( Calendar (..)
    , Reservation (..)
    , User (..)
    , doMigrations
    , runDatabase
    ) where

import Control.Monad.Reader ( MonadIO
                            , MonadReader
                            , asks
                            , liftIO
                            )
import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON
                     , Options (..)
                     )
import Database.Persist.Sql ( SqlPersistT
                            , runSqlPool
                            , runMigration
                            )
import Database.Persist.TH ( mkMigrate
                           , mkPersist
                           , persistLowerCase
                           , share
                           , sqlSettings
                           )
import Data.Text ( Text )
import Data.Time ( UTCTime )

import Configuration ( Configuration (..) )
import Utils ( formatJsonField )


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Calendar json
    name Text
    description Text Maybe default=NULL
    deriving Eq Show

User json
  firstName Text
  lastName Text
  email Text
  UniqueEmail email
  deriving Eq Show

UserCalendar json
    owner UserId
    calendar CalendarId
    UniqueUserCalendar owner calendar

Reservation json
    name Text Maybe default=NULL
    description Text Maybe default=NULL
    calendar CalendarId
    startingDate UTCTime
    endingDate UTCTime
    deriving Eq Show

UserReservation json
    owner UserId
    reservation ReservationId
    UniqueUserReservation owner reservation
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDatabase :: (MonadReader Configuration m, MonadIO m) => SqlPersistT IO b -> m b
runDatabase query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
