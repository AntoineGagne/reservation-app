{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Calendar ( Calendar
                , Reservation
                )
import Utils ( formatJsonField )


data User = User
  { userId        :: Integer
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "user" } ''User)

type API = "users" :> Get '[JSON] [User]

type UserRoutes
    = "users" :> Capture "userid" :> Get '[JSON] User

type CalendarRoutes
    = Get '[JSON] [Calendar]
  :<|> Capture "calendarid" Integer :> Get '[JSON] Calendar
  :<|> PostCreated '[JSON] [Calendar]
  :<|> Capture "calendarid" Integer :> Put '[JSON] Calendar
  :<|> Delete '[JSON] [Calendar]
  :<|> Capture "calendarid" Integer :> Delete '[JSON] Calendar

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
