{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Tutorial.Splices where

import Control.Lens
import Prelude hiding ((++))
import Heist
import Heist.Interpreted
import Snap.Plus
import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P
import qualified Step.Splices
import qualified Dependency.Splices
import qualified Data.Configurator as C

import Tutorial.Types
import Tutorial.Queries

import Application

entitySplice :: TutorialEntity -> Splices (Splice AppHandler)
entitySplice entity@(Entity key (Tutorial x' y' title' iconPath' publish')) = do
  "tutorialId" ## textSplice $ P.showKey key
  "tutorialX" ## textSplice $ tshow x'
  "tutorialY" ## textSplice $ tshow y'
  "tutorialTitle" ## textSplice title'
  "tutorialPublish" ## textSplice $ tshow publish'
  "tutorialIconPath" ## do
    conf' <- use conf
    defaultIconPath' <- liftIO (C.require conf' "default-icon-path")
    textSplice $ maybe defaultIconPath' pack iconPath'
  "tutorialStepNewPath" ## textSplice $ tutorialStepNewPath entity
  "tutorialDeletePath" ## textSplice $ tutorialDeletePath entity
  "tutorialSteps" ## do steps <- lift $ lookupTutorialSteps entity
                        mapSplices (runChildrenWith . Step.Splices.entitySplice) steps
  "tutorialDependencies" ## do dependencies <- lift $ lookupTutorialDependencies entity
                               mapSplices (runChildrenWith . Dependency.Splices.splice entitySplice) dependencies
