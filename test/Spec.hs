{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Api (app)

import qualified Utils


main :: IO ()
main = mapM_ hspec [utilsSpec]

-- spec :: Spec
-- spec = with (return app) $ parallel $
--     describe "GET /users" $ do
--         it "responds with 200" $
--             get "/users" `shouldRespondWith` 200
--         it "responds with [User]" $ do
--             let users = "[{\"first_name\":\"Isaac\",\"last_name\":\"Newton\",\"email\":\"isaac.newton@physicist.org\"},{\"first_name\":\"Albert\",\"last_name\":\"Einstein\",\"email\":\"albert.einstein@physicist.org\"}]"
--             get "/users" `shouldRespondWith` users

calendarRoutesSpec :: Spec
calendarRoutesSpec = undefined

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
