{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

import qualified Snap.Plus     as Snap
import           Snap.Test.BDD

import           Application
import           Site

main :: IO ()
main = runSnapTests (defaultConfig {reportGenerators = [consoleReport, linuxDesktopReport]})
                    (Snap.route routes) app $ name "account" accountsTests


accountsTests :: SnapTesting App ()
accountsTests = return ()
