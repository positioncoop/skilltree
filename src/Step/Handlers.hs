{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Step.Handlers where

import           Database.Persist
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Plus.Handlers
import           Snap.Plus.Paths
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent (runPersist)
import           Text.Digestive.Heist
import           Text.Digestive.Snap     (runForm)

import           Snap.Plus.Forms
import           Step.Form
import           Step.Types
import           Tutorial.Types

import           Application

nestedStepResource :: TutorialEntity -> Resource Step
nestedStepResource t = Resource pass (authorize $ newH t) (const pass) (const pass) (const pass) [] []

stepResource :: Resource Step
stepResource = Resource pass pass (const pass) (authorize . editH) (authorize . deleteH) [] []

newH :: TutorialEntity -> AppHandler ()
newH tutorial@(Entity key _) = do
  response <- runForm "new" (Step.Form.newForm key)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just step) -> do
      void $ runPersist $ insert step
      redirect $ tutorialEditPath tutorial

editH :: StepEntity -> AppHandler ()
editH (Entity stepKey step) = do
  response <- runMultipartForm "edit-step" (Step.Form.editForm step)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just _step) -> do
      runPersist $ replace stepKey _step
      redirect $ editPath $ stepTutorialId step

deleteH :: StepEntity -> AppHandler ()
deleteH (Entity stepKey _) = do
  runPersist $ delete stepKey
  redirectReferer
