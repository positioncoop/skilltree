{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative

import           Snap.Plus           (route)
import           Test.Hspec
import           Test.Hspec.Snap

import           Application
import           Site

main :: IO ()
main = hspec $ do
        describe "basic tests" $ snap (route routes) app $ do
          it "should redirect from index to /tutorials" $
            get "/" >>= should300To "tutorials"
