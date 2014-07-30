{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Step.Handlers where

import Prelude hiding ((++))
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Snap (liftIO)
import Snap.Core hiding (redirect)
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent (runPersist)
import qualified Snap.Snaplet.Persistent as Persistent
import Snap.Extras.JSON
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist
import Control.Monad.Trans.Maybe


import Step.Form
import Step.Types
import Tutorial.Types
import SnapPrelude
import Forms

import Application

routeWithoutTutorial :: [(ByteString, AppHandler ())]
routeWithoutTutorial = [(":id", handler)]
  where handler = do key <- getParam' "id" :: AppHandler (Key Step)
                     step <- require $ runPersist $ get key
                     let tkey = Persistent.mkKey $ stepTutorialId step
                     tut <- require $ runPersist $ get tkey
                     stepHandler (Entity tkey tut) (Entity key step)

routes :: TutorialEntity -> [(ByteString, AppHandler ())]
routes tutorial = [("new", ifTop $ newH tutorial)
                  ]

stepHandler :: TutorialEntity -> StepEntity -> AppHandler ()
stepHandler tutorial (Entity stepKey step) =
  do route [("edit", ifTop $ editH tutorial (Entity stepKey step))
           ,("delete", ifTop $ deleteH tutorial (Entity stepKey step))]

newH :: TutorialEntity -> AppHandler ()
newH tutorial@(Entity key _) = do
  response <- runForm "new" (Step.Form.newForm $ Persistent.mkInt key)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just step) -> do
      runPersist $ insert step
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
