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

tutorialEditPath :: Key Tutorial -> Text
tutorialEditPath tutorialId = (tutorialPath tutorialId) ++ "/edit"

tutorialPath :: Key Tutorial -> Text
tutorialPath tutorialId = "/tutorials/" ++ (P.showKey tutorialId)

tutorialsSplice :: [TutorialEntity] -> Splices (Splice AppHandler)
tutorialsSplice tutorials = "tutorials" ## mapSplices (runChildrenWith . tutorialEntitySplice) tutorials

tutorialSplice :: Tutorial -> Splices (Splice AppHandler)
tutorialSplice (Tutorial _x _y _title) = do
  "tutorialTitle" ## textSplice _title
  "tutorialX" ## textSplice $ tshow _x
  "tutorialY" ## textSplice $ tshow _y

tutorialEntitySplice :: TutorialEntity -> Splices (Splice AppHandler)
tutorialEntitySplice (Entity _id _tutorial) = do
  "editLink" ## textSplice $ tutorialEditPath _id
  "tutorialLink" ## textSplice $ tutorialPath _id
  tutorialSplice _tutorial
