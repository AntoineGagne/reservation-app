{-# LANGUAGE TemplateHaskell #-}

module Calendar
    (
    ) where

import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON )
import Data.Text ( Text )


data Calendar = Calendar
    { calendarId :: Integer
    , calendarName :: Text
    , calendarOwner :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Calendar)
