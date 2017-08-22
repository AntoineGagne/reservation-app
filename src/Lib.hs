{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Database.Persist.Sql ( ConnectionPool
                            , runSqlPersistMPool
                            )
import Data.Time ( UTCTime )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Model ( Calendar
             , Reservation
             , SortReservationBy
             , User (..)
             )
import Utils ( formatJsonField )


type API = "users" :> Get '[JSON] [User]

type UserEndpoint
    = "users" :> Capture "userid" :> Get '[JSON] User

type CalendarEndpoint
    = "calendars" :> (    Get '[JSON] [Calendar]
                     :<|> ReqBody '[JSON] Calendar :> PostCreated '[JSON] [Calendar]
                     :<|> Delete '[JSON] [Calendar]
                     :<|> Capture "calendarid" Integer :>
                         (    Get '[JSON] (Maybe Calendar)
                         :<|> ReqBody '[JSON] Calendar :> Put '[JSON] Calendar
                         :<|> Delete '[JSON] Calendar
                         :<|> ReservationEndpoint
                         )
                     )

type ReservationEndpoint
    = "reservation" :> ( QueryParam "min-time" UTCTime :> QueryParam "max-time" UTCTime 
                                                       :> QueryParam "sortby" SortReservationBy
                                                       :> Get '[JSON] [Reservation]
                       :<|> ReqBody '[JSON] Reservation :> PostCreated '[JSON] [Reservation]
                       :<|> Capture "reservationid" Integer :>
                           (    Get '[JSON] (Maybe Reservation)
                           :<|> ReqBody '[JSON] Reservation :> Put '[JSON] Reservation
                           :<|> Delete '[JSON] Reservation
                           )
                       )

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

calendarServer :: ConnectionPool ->  Server CalendarEndpoint
calendarServer pool = undefined
    where
        getCalendars :: IO [User]
        getCalendars = undefined

reservationServer :: ConnectionPool -> Server ReservationEndpoint
reservationServer = undefined

userServer :: ConnectionPool -> Server UserEndpoint
userServer = undefined

users :: [User]
users = [ User "Isaac" "Newton" "isaac.newton@physicist.org"
        , User "Albert" "Einstein" "albert.einstein@physicist.org"
        ]
