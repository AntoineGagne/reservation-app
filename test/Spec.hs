{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.Except ( runExceptT )
import Control.Monad.Reader ( runReaderT )
import Data.CaseInsensitive
import Database.Persist.Sqlite ( runSqlPool )
import Servant ( ServantErr )
import Network.Wai ( Application )
import Network.Wai.Test ( SResponse )
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Api (app)
import Configuration ( Configuration (..)
                     , Environment (..)
                     , makePool
                     )
import Model ( doMigrations
             , Calendar (..)
             )

import qualified Configuration as C
import qualified Utils


setup :: (Configuration -> IO a) -> IO a
setup runTestWith = do
    let environment = Test
    pool <- makePool Test
    runSqlPool doMigrations pool
    runTestWith Configuration { getPool = pool, getEnvironment = environment }

applicationToIO :: Configuration -> C.Application a -> IO (Either ServantErr a)
applicationToIO configuration = runExceptT . flip runReaderT configuration . C.runApp

main :: IO ()
main = do
    application <- setup (pure . app)
    mapM_ hspec [utilsSpec, calendarRoutesSpec application]

calendarRoutesSpec :: Application -> Spec
calendarRoutesSpec application = with (pure application) $ do
    describe "GET /calendars" $ do
        it "responds with 200" $
            get "/calendars" `shouldRespondWith` 200
        it "responds with 404 when no calendar with the given ID can be found" $
            get "/calendars/1001" `shouldRespondWith` 404
    describe "DELETE /calendars" $
        it "responds with 404 when no calendar with the given ID can be found" $
            delete "/calendars/1001" `shouldRespondWith` 404
    describe "POST /calendars" $
        it "responds with 201" $
            let calendar = [json|{name: "Test", description: "Test description"}|]
                in jsonPost "/calendars" calendar `shouldRespondWith` 201

jsonPost :: BS.ByteString -> LBS.ByteString -> WaiSession SResponse
jsonPost = jsonRequest "POST"

jsonRequest :: BS.ByteString -> BS.ByteString -> LBS.ByteString -> WaiSession SResponse
jsonRequest method path = request method path [(mk "Content-Type", "application/json")]

reservationRoutesSpec :: Spec
reservationRoutesSpec = undefined

utilsSpec :: Spec
utilsSpec = parallel $ do
    describe "stripPrefix" $ do
        it "strips the prefix when it is present" $ do
            let a = "userFirstName"
            Utils.stripPrefix "user" a `shouldNotContain` "user"
        it "returns the same string when the prefix is not present" $ do
            let a = "firstName"
            Utils.stripPrefix "user" a `shouldBe` a
    describe "toSnakeCase" $ do
        it "returns the camel cased version of the string" $
            Utils.toSnakeCase "firstName" `shouldBe` "first_name"
        it "does not contain a prefixed underscore when starting with upper case" $
            Utils.toSnakeCase "FirstName" `shouldBe` "first_name"
    describe "formatJsonField" $
        it "returns snake cased string without the given prefix" $
            Utils.formatJsonField "user" "userFirstName" `shouldBe` "first_name"
