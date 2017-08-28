module User
    ( getUser
    ) where

import Database.Persist.Sql ( get
                            , Key
                            , toSqlKey
                            )
import Database.Persist.Sqlite ( Entity (..) )
import Servant ( err404
               , throwError
               )

import qualified Configuration as C
import Model ( User (..)
             , runDatabase
             )


getUser :: Integer -> C.Application (Entity User)
getUser k = do
    maybeUser <- runDatabase . get $ userId
    case maybeUser of
        Just user -> pure $ Entity userId user
        Nothing -> throwError err404
    where
        userId = toUserId k
        toUserId :: Integer -> Key User
        toUserId = toSqlKey . fromIntegral
