module Utils
    ( formatJsonField
    , stripPrefix
    , toSnakeCase
    ) where

import Data.Char ( isUpper
                 , toLower
                 )
import Data.Maybe ( fromMaybe )
import qualified Data.List as List

formatJsonField :: String -> String -> String
formatJsonField p = toSnakeCase . stripPrefix p

toSnakeCase :: String -> String
toSnakeCase s = case concatMap f s of
                    ('_':xs) -> xs
                    xs -> xs
    where 
        f c
            | isUpper c = ['_', toLower c]
            | otherwise = [c]

stripPrefix :: String -> String -> String
stripPrefix p s = fromMaybe s $ List.stripPrefix p s
