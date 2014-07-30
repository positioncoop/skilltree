{-# LANGUAGE OverloadedStrings, GADTs #-}

module Test where

import Control.Lens (use)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Control.Applicative
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import System.Random (randomIO)

import Snap.Core hiding (redirect)
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Test.BDD

import SnapPrelude
import Application
import Site

main :: IO ()
main = runSnapTests (defaultConfig {reportGenerators = [consoleReport, linuxDesktopReport]})
                    (route routes) app $ do
         (name "account" accountsTests)


accountsTests :: SnapTesting App ()
accountsTests = return ()
