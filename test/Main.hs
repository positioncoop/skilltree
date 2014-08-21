{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec.WebDriver
import Snap.Snaplet.Test
import qualified Data.Map as M
import Snap.Test (get)
import Snap.Snaplet.PostgresqlSimple

import Site
import Application

runAppFn :: AppHandler a -> IO a
runAppFn handler = do result <- evalHandler (Just "test") (get "/" M.empty) handler app
                      case result of
                        Left err -> error (show err)
                        Right v -> return v

main :: IO ()
main = hspec $
  describe "First test" $
    session "Main page" $ using Chrome $ do
      runIO $ runAppFn $ execute_ "TRUNCATE tutorial, snap_auth_user CASCADE"
      it "opens the page" $ runWD $ do
        openPage "http://localhost:8001/auth/signup"

        sendKeys "a@boodle.com" =<< findElem (ById "signup.email.address")
        sendKeys "a@boodle.com" =<< findElem (ById "signup.email.confirm")
        sendKeys "pass" =<< findElem (ById "signup.password")
        submit =<< findElem (ByCSS "form")

        openPage "http://localhost:8001/auth/login"
        sendKeys "a@boodle.com" =<< findElem (ById "login.email.address")
        sendKeys "pass" =<< findElem (ById "login.password")
        submit =<< findElem (ByCSS "form")

        e <- findElem $ ByCSS "p"
        e `shouldHaveText` "Hi, a@boodle.com Logout"
