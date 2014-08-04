{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Step.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent (runPersist)
import qualified Snap.Snaplet.Persistent as Persistent
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Step.Form
import Step.Types
import Tutorial.Types
import Snap.Plus.Forms

import Application

routeWithoutTutorial :: [(Text, AppHandler ())]
routeWithoutTutorial = [(":id", requireUser auth pass handler)]
  where handler = do key <- getParam' "id" :: AppHandler (Key Step)
                     step <- require $ runPersist $ get key
                     let tkey = Persistent.mkKey $ stepTutorialId step
                     tut <- require $ runPersist $ get tkey
                     stepHandler (Entity tkey tut) (Entity key step)

routes :: TutorialEntity -> [(Text, AppHandler ())]
routes tutorial = [("new", ifTop $ requireUser auth pass $ newH tutorial)
                  ]

stepHandler :: TutorialEntity -> StepEntity -> AppHandler ()
stepHandler tutorial (Entity stepKey step) =
  route [("edit", ifTop $ editH tutorial (Entity stepKey step))
        ,("delete", ifTop $ deleteH tutorial (Entity stepKey step))]

newH :: TutorialEntity -> AppHandler ()
newH tutorial@(Entity key _) = do
  response <- runForm "new" (Step.Form.newForm $ Persistent.mkInt key)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just step) -> do
      void $ runPersist $ insert step
      redirect $ tutorialEditPath tutorial

editH :: TutorialEntity -> StepEntity -> AppHandler ()
editH tutorial (Entity stepKey step) = do
  response <- runMultipartForm "edit-step" (Step.Form.editForm step)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just _step) -> do
      runPersist $ replace stepKey _step
      redirect $ tutorialEditPath tutorial

deleteH :: TutorialEntity -> StepEntity -> AppHandler ()
deleteH entity (Entity stepKey _) = do
  runPersist $ delete stepKey
  redirect $ tutorialEditPath entity
