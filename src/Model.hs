{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , SortReservationBy (..)
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


data SortReservationBy = Month
                       | Week
                       | Year

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Calendar
    name Text
    description Text Maybe default=NULL
    deriving Eq Show

User
  firstName Text
  lastName Text
  email Text
  UniqueEmail email
  deriving Eq Show

UserCalendar
    owner UserId
    calendar CalendarId
    UniqueUserCalendar owner calendar

Reservation
    name Text Maybe default=NULL
    description Text Maybe default=NULL
    calendar CalendarId
    startingDate UTCTime
    endingDate UTCTime
    deriving Eq Show

UserReservation
    owner UserId
    reservation ReservationId
    UniqueUserReservation owner reservation
|]

$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "calendar" } ''Calendar)
$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "user" } ''User)
$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "reservation" } ''Reservation)

runDatabase :: (MonadReader Configuration m, MonadIO m) => SqlPersistT IO b -> m b
runDatabase query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
