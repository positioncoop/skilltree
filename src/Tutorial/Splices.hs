{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Splices where

import Prelude hiding ((++))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted

import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P

import Tutorial.Types
import Helpers
import Application

tutorialEditPath :: Int -> Text
tutorialEditPath tutorialId = (tutorialPath tutorialId) ++ "/edit"

tutorialPath :: Int -> Text
tutorialPath tutorialId = "/tutorials/" ++ (tshow tutorialId)

tutorialsSplice :: [Tutorial] -> Splices (Splice AppHandler)
tutorialsSplice tutorials = "tutorials" ## mapSplices (runChildrenWith . tutorialSplice) tutorials

tutorialSplice :: Tutorial -> Splices (Splice AppHandler)
tutorialSplice (Tutorial' _id _x _y _title) = do
  "editLink" ## textSplice $ tutorialEditPath _id
  "tutorialLink" ## textSplice $ tutorialPath _id
  "tutorialTitle" ## textSplice _title
  "tutorialX" ## textSplice $ tshow _x
  "tutorialY" ## textSplice $ tshow _y
