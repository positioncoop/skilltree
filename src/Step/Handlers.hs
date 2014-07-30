{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Step.Handlers where

import Prelude hiding ((++))
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Snap (liftIO)
import Snap.Core
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
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

routeWithoutTutorial :: [(ByteString, AppHandler ())]
routeWithoutTutorial = [(":id", handler >>= maybe pass (uncurry stepHandler))]
  where handler = runMaybeT $
          do key <- MaybeT $ stepKeyParam "id"
             step <- MaybeT $ runPersist $ get key
             let tkey = mkKey $ stepTutorialId step
             tut <- MaybeT $ runPersist $ get tkey
             return (Entity tkey tut, Just $ Entity key step)

routes :: TutorialEntity -> [(ByteString, AppHandler ())]
routes tutorial = [("new", ifTop $ newH tutorial)
                  ,(":id", stepHandler tutorial Nothing)]

stepHandler :: TutorialEntity -> Maybe StepEntity -> AppHandler ()
stepHandler tutorial mstep =
  do (Entity stepKey step) <- fromMaybe lookup (return <$> mstep)
     route [("edit", ifTop $ editH tutorial (Entity stepKey step))
                                  ,("delete", ifTop $ deleteH tutorial (Entity stepKey step))]
  where lookup = do maybeStepKey <- stepKeyParam "id"
                    case maybeStepKey of
                      Nothing -> pass
                      Just stepKey -> do
                        maybeStep <- runPersist $ get stepKey
                        case maybeStep of
                          Nothing -> pass
                          Just step -> return (Entity stepKey step)


home :: AppHandler ()
home = redirect $ T.encodeUtf8 $ "/"

newH :: TutorialEntity -> AppHandler ()
newH tutorial = do
  response <- runForm "new" (Step.Form.newForm 0)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just step) -> do
      runPersist $ insert step
      home

editH :: TutorialEntity -> StepEntity -> AppHandler ()
editH tutorial (Entity stepKey step) = do
  response <- runMultipartForm "edit-step" (Step.Form.editForm step)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just _step) -> do
      runPersist $ replace stepKey _step
      redirect $ "/steps/" ++ (showKeyBS stepKey) ++ "/edit"

deleteH :: TutorialEntity -> StepEntity -> AppHandler ()
deleteH tutorial (Entity stepKey _) = do
  runPersist $ delete stepKey
  home

stepKeyParam :: MonadSnap m => ByteString -> m (Maybe (Key Step))
stepKeyParam name = fmap (fmap mkKeyBS) (getParam name)
