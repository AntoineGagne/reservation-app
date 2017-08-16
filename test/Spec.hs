{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Lib (app)

import qualified Utils


main :: IO ()
main = mapM_ hspec [utilsSpec, spec]

spec :: Spec
spec = with (return app) $
    describe "GET /users" $ do
        it "responds with 200" $
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            get "/users" `shouldRespondWith` users

utilsSpec :: Spec
utilsSpec = parallel $ do
    describe "stripPrefix" $ do
        it "should strip the prefix when it is present" $ do
            let a = "userFirstName"
            Utils.stripPrefix "user" a `shouldNotContain` "user"
        it "should return the same string when the prefix is not present" $ do
            let a = "firstName"
            Utils.stripPrefix "user" a `shouldBe` a
    describe "toSnakeCase" $ do
        it "should return the camel cased version of the string" $
            Utils.toSnakeCase "firstName" `shouldBe` "first_name"
        it "should not contain a prefixed underscore when starting with upper case" $
            Utils.toSnakeCase "FirstName" `shouldBe` "first_name"
    describe "formatJsonField" $
        it "should return snake cased string without the given prefix" $
            Utils.formatJsonField "user" "userFirstName" `shouldBe` "first_name"
