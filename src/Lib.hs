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
import Data.Time ( UTCTime )
import Database.Persist.Sql ( insert
                            , selectList
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
               )

import Model ( Calendar
             , Reservation
             , User (..)
             , runDatabase
             )
import Configuration ( Configuration (..) )
import qualified Configuration as C


type API = UserAPI :<|> CalendarAPI

type UserAPI
    = "users" :> Capture "userid" Integer :> Get '[JSON] User

type CalendarAPI
    = "calendars" :> (    Get '[JSON] [Entity Calendar]
                     :<|> ReqBody '[JSON] Calendar :> PostCreated '[JSON] (Entity Calendar)
                     :<|> Delete '[JSON] [Entity Calendar]
                     :<|> Capture "calendarid" Integer :>
                         (    Get '[JSON] (Maybe (Entity Calendar))
                         :<|> ReqBody '[JSON] Calendar :> Put '[JSON] (Entity Calendar)
                         :<|> Delete '[JSON] (Entity Calendar)
                         :<|> ReservationAPI
                         )
                     )

type ReservationAPI
    = "reservation" :> ( QueryParam "min-time" UTCTime :> QueryParam "max-time" UTCTime 
                                                       :> Get '[JSON] [Entity Reservation]
                       :<|> ReqBody '[JSON] Reservation :> PostCreated '[JSON] [Entity Reservation]
                       :<|> Capture "reservationid" Integer :>
                           (    Get '[JSON] (Maybe (Entity Reservation))
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
calendarServer = getCalendars :<|> insertCalendar :<|> undefined

getCalendars :: C.Application [Entity Calendar]
getCalendars = runDatabase $ selectList [] []

insertCalendar :: Calendar -> C.Application (Entity Calendar)
insertCalendar calendar = do
    calendarId <- runDatabase $ insert calendar
    pure $ Entity calendarId calendar

userServer :: ServerT UserAPI C.Application
userServer = undefined

reservationServer :: ServerT ReservationAPI C.Application
reservationServer = undefined
