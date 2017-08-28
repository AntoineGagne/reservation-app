module Calendar
    ( deleteCalendar
    , getCalendar
    , getCalendars
    , insertCalendar
    , replaceCalendar
    ) where

import Database.Persist.Sql ( delete
                            , get
                            , insert
                            , replace
                            , selectList
                            , toSqlKey
                            , Key
                            )
import Database.Persist.Sqlite ( Entity (..) )
import Servant ( err404
               , throwError
               )

import Model ( Calendar
             , Reservation
             , User (..)
             , runDatabase
             )
import Configuration ( Configuration (..) )
import qualified Configuration as C


getCalendars :: C.Application [Entity Calendar]
getCalendars = runDatabase $ selectList [] []

insertCalendar :: Calendar -> C.Application (Entity Calendar)
insertCalendar calendar = do
    calendarId <- runDatabase $ insert calendar
    pure $ Entity calendarId calendar

getCalendar :: Integer -> C.Application (Entity Calendar)
getCalendar k = do
    maybeCalendar <- runDatabase . get $ calendarId
    case maybeCalendar of
        Just calendar -> pure $ Entity calendarId calendar
        Nothing -> throwError err404
    where
        calendarId = toCalendarId k

toCalendarId :: Integer -> Key Calendar
toCalendarId = toSqlKey . fromIntegral

replaceCalendar :: Integer -> Calendar -> C.Application (Entity Calendar)
replaceCalendar k calendar = do
    exists <- runDatabase . get $ calendarId
    case exists of
        Just _ -> do
            runDatabase $ replace calendarId calendar
            pure $ Entity calendarId calendar
        Nothing -> throwError err404
    where
        calendarId = toCalendarId k

deleteCalendar :: Integer -> C.Application (Entity Calendar)
deleteCalendar k = do
    maybeCalendar <- runDatabase . get $ calendarId
    case maybeCalendar of
        Just calendar -> do
            runDatabase . delete $ calendarId
            pure $ Entity calendarId calendar
        Nothing -> throwError err404
    where
        calendarId = toCalendarId k
