{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Tutorial.Splices where

import Prelude hiding ((++))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted
import Snap
import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P
import qualified Step.Splices
import SnapPrelude

import Tutorial.Types
import Tutorial.Queries

import Application

entitySplice :: TutorialEntity -> Splices (Splice AppHandler)
entitySplice entity@(Entity _id (Tutorial _x _y _title _iconPath)) = do
  "tutorialId" ## textSplice $ P.showKey _id
  "tutorialX" ## textSplice $ tshow _x
  "tutorialY" ## textSplice $ tshow _y
  "tutorialTitle" ## textSplice _title
  "tutorialIconPath" ## textSplice $ maybe "" T.pack _iconPath
  "tutorialStepNewPath" ## textSplice $ tutorialStepNewPath entity
  "tutorialDeletePath" ## textSplice $ tutorialDeletePath entity
  "tutorialSteps" ## do steps <- lift $ lookupTutorialSteps entity
                        mapSplices (runChildrenWith . Step.Splices.entitySplice) steps

