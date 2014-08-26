{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Tutorial.Splices where

import Control.Lens
import Prelude hiding ((++))
import Heist
import Heist.Interpreted
import Snap.Plus
import Snap.Plus.Paths
import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P
import qualified Step.Splices
import qualified Data.Configurator as C

import Tutorial.Types
import Tutorial.Queries

import Application

entitySplice :: TutorialEntity -> Splices (Splice AppHandler)
entitySplice entity@(Entity key (Tutorial x' y' title' iconPath' publish')) = do
  "siteLogoPath" ## do
    conf' <- use conf
    siteLogoPath' <- liftIO (C.require conf' "site-logo-path")
    textSplice $ siteLogoPath'
  "tutorialId" ## textSplice $ P.showKey key
  "tutorialX" ## textSplice $ tshow x'
  "tutorialY" ## textSplice $ tshow y'
  "tutorialTitle" ## textSplice title'
  "tutorialPublish" ## textSplice $ tshow publish'
  "tutorialDefaultIconPath" ## do
    conf' <- use conf
    tutorialDefaultIconPath' <- liftIO (C.require conf' "tutorial-default-icon-path")
    textSplice $ maybe tutorialDefaultIconPath' pack iconPath'
  "tutorialStepNewPath" ## textSplice $ tutorialStepNewPath entity
  "tutorialDeletePath" ## textSplice $ tutorialDeletePath entity
  "tutorialEditPath" ## textSplice $ editPath key
  "tutorialShowPath" ## textSplice $ showPath key
  "tutorialSteps" ## do steps <- lift $ lookupTutorialSteps entity
                        mapSplices (runChildrenWith . Step.Splices.entitySplice) steps
