{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( app
    ) where

import Control.Monad.Except ( ExceptT )
import Control.Monad.Reader ( ReaderT
                            , runReaderT
                            )
import Control.Monad.Reader.Class
import Data.Int ( Int64 (..) )
import Data.Time ( UTCTime )
import Database.Persist.Sql ( delete
                            , get
                            , insert
                            , replace
                            , selectList
                            , toSqlKey
                            , Key
                            )
import Database.Persist.Sqlite ( Entity (..) )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( Capture
               , Delete
               , Get
               , Handler (..)
               , JSON
               , PostCreated
               , Proxy (..)
               , Put
               , QueryParam
               , ReqBody
               , ServantErr (..)
               , Server
               , ServerT (..)
               , (:<|>) (..)
               , (:>)
               , (:~>) (..)
               , enter
               , serve
               , err404
               , throwError
               )

import Model ( Calendar
             , Reservation
             , User (..)
             , runDatabase
             )
import Configuration ( Configuration (..) )
import qualified Configuration as C


type API = UserAPI :<|> CalendarAPI :<|> ReservationAPI

type UserAPI
    = "users" :> Capture "userid" Integer :> Get '[JSON] User

type CalendarAPI
    = "calendars" :> (    Get '[JSON] [Entity Calendar]
                     :<|> ReqBody '[JSON] Calendar :> PostCreated '[JSON] (Entity Calendar)
                     :<|> Capture "calendarid" Integer :>
                         (    Get '[JSON] (Entity Calendar)
                         :<|> ReqBody '[JSON] Calendar :> Put '[JSON] (Entity Calendar)
                         :<|> Delete '[JSON] (Entity Calendar)
                         )
                     )

type ReservationAPI
    = "calendars" :> Capture "calendarid" Integer
                  :> "reservation" 
                  :> ( QueryParam "min-time" UTCTime :> QueryParam "max-time" UTCTime 
                                                     :> Get '[JSON] [Entity Reservation]
                     :<|> ReqBody '[JSON] Reservation :> PostCreated '[JSON] [Entity Reservation]
                     :<|> Capture "reservationid" Integer :>
                         (    Get '[JSON] (Entity Reservation)
                         :<|> ReqBody '[JSON] Reservation :> Put '[JSON] (Entity Reservation)
                         :<|> Delete '[JSON] (Entity Reservation)
                         )
                     )

app :: Configuration -> Application
app configuration = serve api $ appToServer configuration

appToServer :: Configuration -> Server API
appToServer configuration = enter (convertApp configuration) server

convertApp :: Configuration -> C.Application :~> Handler
convertApp configuration = NT $ Handler . flip runReaderT configuration . C.runApp

api :: Proxy API
api = Proxy

server :: ServerT API C.Application
server = undefined

calendarServer :: ServerT CalendarAPI C.Application
calendarServer = getCalendars
            :<|> insertCalendar
            :<|> handler
    where
        handler calendarid = getCalendar calendarid
                        :<|> replaceCalendar calendarid
                        :<|> deleteCalendar calendarid

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

userServer :: ServerT UserAPI C.Application
userServer = undefined

reservationServer :: ServerT ReservationAPI C.Application
reservationServer = undefined
