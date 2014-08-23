{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Step.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Plus.Paths
import Snap.Plus.Handlers
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent (runPersist)
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Step.Form
import Step.Types
import Tutorial.Types
import Snap.Plus.Forms

import Application

nestedStepResource :: TutorialEntity -> Resource Step
nestedStepResource t = SimpleResource pass (authorize $ newH t) (const pass) (const pass) (const pass)

stepResource :: Resource Step
stepResource = SimpleResource pass pass (const pass) (authorize . editH) (authorize . deleteH)

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
