{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Tutorial.Splices where

import           Control.Lens
import qualified Data.Configurator       as C
import           Database.Persist.Types
import           Heist
import           Heist.Interpreted
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Plus.Paths
import qualified Snap.Snaplet.Persistent as P
import qualified Step.Splices

import           Tutorial.Queries
import           Tutorial.Types

import           Application

entitySplice :: TutorialEntity -> Splices (Splice AppHandler)
entitySplice entity@(Entity key (Tutorial x' y' title' iconPath' publish')) = do
  "tutorialId" ## textSplice $ P.showKey key
  "tutorialX" ## textSplice $ tshow x'
  "tutorialY" ## textSplice $ tshow y'
  "tutorialTitle" ## textSplice title'
  "tutorialPublish" ## textSplice $ tshow publish'
  "tutorialIconPath" ## do
    conf' <- use conf
    tutorialDefaultIconPath' <- liftIO (C.require conf' "tutorial-default-icon-path")
    textSplice $ maybe tutorialDefaultIconPath' pack iconPath'
  "tutorialStepNewPath" ## textSplice $ tutorialStepNewPath entity
  "tutorialDeletePath" ## textSplice $ tutorialDeletePath entity
  "tutorialEditPath" ## textSplice $ editPath key
  "tutorialShowPath" ## textSplice $ showPath key
  "tutorialSteps" ## do steps <- lift $ lookupTutorialSteps entity
                        mapSplices (runChildrenWith . Step.Splices.entitySplice) steps
