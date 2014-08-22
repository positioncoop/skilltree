{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((++))
import Snap.Plus
import Test.Hspec.WebDriver
import Snap.Snaplet.Test
import qualified Data.Map as M
import Snap.Test (get)
import Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Site
import Application

runAppFn :: AppHandler a -> IO a
runAppFn handler = do result <- evalHandler (Just "test") (get "/" M.empty) handler app
                      case result of
                        Left err -> error (show err)
                        Right v -> return v

mkSS pth = do ss <- screenshot
              liftIO $ B.writeFile pth . LB.toStrict $ ss

signup = do openPage "http://localhost:8001/auth/signup"
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

createTutorial = do svg <- findElem (ByCSS "svg")
                    moveToCenter svg
                    clickWith LeftButton
                    e <- findElem (ByCSS "g.tutorial")
                    e `shouldBeTag` "g"

moveTutorial = do m <- findElem (ByCSS "g.tutorial .move-icon")
                  moveToCenter m
                  clickWith LeftButton
                  t <- findElem (ByCSS "g.tutorial")
                  (x, y) <- elemPos t
                  moveTo (100, 100)
                  clickWith LeftButton
                  -- NOTE(dbp 2014-08-21): I'm not sure how to delay, so refresh to force it.
                  refresh
                  t2 <- findElem (ByCSS ".tutorial")
                  (x', y') <- elemPos t2
                  shouldBe True (x /= x' && y /= y')

createDep = do
  click =<< findElem (ByCSS "svg")
  refresh
  [tutorial1, tutorial2] <- findElems (ByCSS "g.tutorial")
  click =<< findElemFrom tutorial1 (ByCSS $ ".fa-long-arrow-right")
  click =<< findElemFrom tutorial2 (ByCSS $ ".fa-bullseye")
  refresh
  deps <- findElems (ByCSS "line")
  shouldBe 1 (length deps)

deleteDep = do
  [tutorial1, tutorial2] <- findElems (ByCSS "g.tutorial")
  click =<< findElemFrom tutorial1 (ByCSS $ ".fa-long-arrow-right")
  click =<< findElemFrom tutorial2 (ByCSS $ ".fa-bullseye")
  refresh
  deps <- findElems (ByCSS "line")
  shouldBe 0 (length deps)

-- Structured Haskell Mode hates using... don't know why.
use = using

main :: IO ()
main = hspec $ describe "Skilltree Selenium" $ session "Main page" $ use Chrome $
        do runIO $ runAppFn $ execute_ "TRUNCATE tutorial, snap_auth_user CASCADE"
           it "setup" $ runWD $ setImplicitWait 1000
           it "signs up and logs in" $ runWD signup
           it "creates a tutorial" $ runWD createTutorial
           it "moves a tutorial" $ runWD moveTutorial
           it "creates a dependency" $ runWD createDep
           it "deletes a dependency" $ runWD deleteDep