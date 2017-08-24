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
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( Capture
               , Delete
               , Get
               , JSON
               , PostCreated
               , Proxy (..)
               , Put
               , QueryParam
               , ReqBody
               , Server
               , (:<|>)
               , (:>)
               , (:~>)
               , serve
               )

import Model ( Calendar
             , Reservation
             , SortReservationBy
             , User (..)
             )


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

calendarServer :: Server CalendarEndpoint
calendarServer = undefined
    where
        getCalendars :: IO [User]
        getCalendars = undefined

reservationServer :: Server ReservationEndpoint
reservationServer = undefined

userServer :: Server UserEndpoint
userServer = undefined

users :: [User]
users = [ User "Isaac" "Newton" "isaac.newton@physicist.org"
        , User "Albert" "Einstein" "albert.einstein@physicist.org"
        ]
