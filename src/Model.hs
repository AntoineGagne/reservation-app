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

import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON
                     , Options (..)
                     )
import Database.Persist.TH ( mkMigrate
                           , mkPersist
                           , persistLowerCase
                           , share
                           , sqlSettings
                           )
import Data.Text ( Text )
import Data.Time ( UTCTime )

import Utils ( formatJsonField )


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

data SortReservationBy = Month
                       | Week
                       | Year
