{-# LANGUAGE TemplateHaskell #-}

module Calendar
    ( Calendar (..)
    , Reservation (..)
    ) where

import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON
                     , Options (..)
                     )
import Data.Text ( Text )
import Data.Time ( UTCTime )

import Utils ( formatJsonField )


data Calendar = Calendar
    { calendarId :: Integer
    , calendarName :: Text
    , calendarDescription :: Maybe Text
    } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "calendar" } ''Calendar)

data Reservation = Reservation
    { reservationId :: Integer
    , reservationName :: Maybe Text
    , reservationDescription :: Maybe Text
    , reservationCalendarId :: Integer
    , reservationOwnerId :: Integer
    , reservationStartingDate :: UTCTime
    , reservationEndingDate :: UTCTime
    , reservationPayingCustomersNumber :: Integer
    } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "reservation" } ''Reservation)