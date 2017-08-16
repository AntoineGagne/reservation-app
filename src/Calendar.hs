{-# LANGUAGE TemplateHaskell #-}

module Calendar
    (
    ) where

import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON
                     , Options (..)
                     )
import Data.Text ( Text )

import Utils ( formatJsonField )


data Calendar = Calendar
    { calendarId :: Integer
    , calendarName :: Text
    , calendarOwner :: Integer
    } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "calendar" } ''Calendar)
