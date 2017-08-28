{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( app
    ) where

import Control.Monad.Except ( ExceptT )
import Control.Monad.Reader ( ReaderT
                            , runReaderT
                            )
import Control.Monad.Reader.Class
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

import Calendar ( deleteCalendar
                , getCalendar
                , getCalendars
                , insertCalendar
                , replaceCalendar
                )
import Configuration ( Configuration (..) )
import Model ( Calendar
             , Reservation
             , User (..)
             , runDatabase
             )
import User ( getUser )

import qualified Configuration as C


type API = UserAPI :<|> CalendarAPI :<|> ReservationAPI

type UserAPI
    = "users" :> Capture "userid" Integer :> Get '[JSON] (Entity User)

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
server = userServer :<|> calendarServer :<|> reservationServer

calendarServer :: ServerT CalendarAPI C.Application
calendarServer = getCalendars
            :<|> insertCalendar
            :<|> handler
    where
        handler calendarid = getCalendar calendarid
                        :<|> replaceCalendar calendarid
                        :<|> deleteCalendar calendarid

userServer :: ServerT UserAPI C.Application
userServer = getUser

reservationServer :: ServerT ReservationAPI C.Application
reservationServer = undefined
