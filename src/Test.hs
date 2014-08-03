{-# LANGUAGE OverloadedStrings, GADTs #-}

module Test where

import Control.Lens (use)
import qualified Data.Map as M
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import System.Random (randomIO)

import Snap.Plus
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Test.BDD

import Application
import Site

main :: IO ()
main = runSnapTests (defaultConfig {reportGenerators = [consoleReport, linuxDesktopReport]})
                    (route routes) app $ do
         (name "account" accountsTests)


accountsTests :: SnapTesting App ()
accountsTests = return ()
