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
             )
import Configuration ( Configuration (..) )
import qualified Configuration as C


type API = UserAPI :<|> CalendarAPI

type UserAPI
    = "users" :> Capture "userid" Integer :> Get '[JSON] User

type CalendarAPI
    = "calendars" :> (    Get '[JSON] [Calendar]
                     :<|> ReqBody '[JSON] Calendar :> PostCreated '[JSON] [Calendar]
                     :<|> Delete '[JSON] [Calendar]
                     :<|> Capture "calendarid" Integer :>
                         (    Get '[JSON] (Maybe Calendar)
                         :<|> ReqBody '[JSON] Calendar :> Put '[JSON] Calendar
                         :<|> Delete '[JSON] Calendar
                         :<|> ReservationAPI
                         )
                     )

type ReservationAPI
    = "reservation" :> ( QueryParam "min-time" UTCTime :> QueryParam "max-time" UTCTime 
                                                       :> Get '[JSON] [Reservation]
                       :<|> ReqBody '[JSON] Reservation :> PostCreated '[JSON] [Reservation]
                       :<|> Capture "reservationid" Integer :>
                           (    Get '[JSON] (Maybe Reservation)
                           :<|> ReqBody '[JSON] Reservation :> Put '[JSON] Reservation
                           :<|> Delete '[JSON] Reservation
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
calendarServer = undefined

getCalendars :: C.Application [Calendar]
getCalendars = undefined

reservationServer :: ServerT ReservationAPI C.Application
reservationServer = undefined

userServer :: ServerT UserAPI C.Application
userServer = undefined
