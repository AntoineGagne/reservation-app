{-# LANGUAGE TemplateHaskell #-}

module User
    ( User (..)
    ) where

import Data.Aeson ( defaultOptions )
import Data.Aeson.TH ( deriveJSON
                     , Options (..)
                     )
import Data.Text ( Text )

import Utils ( formatJsonField )


data User = User
  { userId        :: Integer
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = formatJsonField "user" } ''User)
