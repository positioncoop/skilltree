{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Step.Handlers where

import Prelude hiding ((++))
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Snap (liftIO)
import Snap.Core
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
import Helpers
import Forms

import Application

lookupStepFromParam :: AppHandler (Maybe StepEntity)
lookupStepFromParam = runMaybeT (do key <- MaybeT $ stepKeyParam "id"
                                    step <- MaybeT $ runPersist $ get key
                                    return (Entity key step))

routeWithoutTutorial :: [(ByteString, AppHandler ())]
routeWithoutTutorial = [(":id", handler >>= maybe pass (uncurry stepHandler))]
  where handler = runMaybeT $
          do (Entity key step) <- MaybeT lookupStepFromParam
             let tkey = Persistent.mkKey $ stepTutorialId step
             tut <- MaybeT $ runPersist $ get tkey
             return (Entity tkey tut, Entity key step)

routes :: TutorialEntity -> [(ByteString, AppHandler ())]
routes tutorial = [("new", ifTop $ newH tutorial)
                  ]

stepHandler :: TutorialEntity -> StepEntity -> AppHandler ()
stepHandler tutorial (Entity stepKey step) =
  do route [("edit", ifTop $ editH tutorial (Entity stepKey step))
           ,("delete", ifTop $ deleteH tutorial (Entity stepKey step))]

home :: AppHandler ()
home = redirect $ T.encodeUtf8 $ "/"

newH :: TutorialEntity -> AppHandler ()
newH tutorial@(Entity key _) = do
  response <- runForm "new" (Step.Form.newForm $ Persistent.mkInt key)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just step) -> do
      runPersist $ insert step
      redirect $ tutorialPath tutorial

editH :: TutorialEntity -> StepEntity -> AppHandler ()
editH _ (Entity stepKey step) = do
  response <- runMultipartForm "edit-step" (Step.Form.editForm step)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just _step) -> do
      runPersist $ replace stepKey _step
      redirect $ "/steps/" ++ (Persistent.showKeyBS stepKey) ++ "/edit"

deleteH :: TutorialEntity -> StepEntity -> AppHandler ()
deleteH _ (Entity stepKey _) = do
  runPersist $ delete stepKey
  home

stepKeyParam :: MonadSnap m => ByteString -> m (Maybe (Key Step))
stepKeyParam name = fmap (fmap Persistent.mkKeyBS) (getParam name)
