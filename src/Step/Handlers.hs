{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Step.Handlers where

import Prelude hiding ((++))
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Data.Aeson
import Snap (liftIO)
import Snap.Core
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Restful
import Snap.Extras.JSON
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Step.Form
import Step.Types
import Helpers
import Forms

import Application

resource :: Resource
resource = Resource "step" "/steps" [] []

crud :: [(CRUD, AppHandler ())]
crud =  [ (RNew, newH)
        , (RCreate, newH)
        , (REdit, editH)
        , (RUpdate, editH)
        ]

home :: AppHandler ()
home = redirect $ T.encodeUtf8 $ "/"

newH :: AppHandler ()
newH = do
  response <- runForm "new" (Step.Form.newForm 0)
  case response of
    (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
    (_, Just step) -> do
      runPersist $ insert step
      home

editH :: AppHandler ()
editH = do
  maybeStepKey <- stepKeyParam "id"
  case maybeStepKey of
    Nothing -> pass
    Just stepKey -> do
      maybeStep <- runPersist $ get stepKey
      case maybeStep of
        Nothing -> pass
        Just step -> do
          response <- runMultipartForm "edit-step" (Step.Form.editForm $ step)
          case response of
            (v, Nothing) -> renderWithSplices "steps/form" (digestiveSplices v)
            (_, Just _step) -> do
              runPersist $ replace stepKey _step
              redirect $ "/steps/" ++ (showKeyBS stepKey) ++ "/edit"

deleteH :: AppHandler ()
deleteH = do
  maybeStepKey <- stepKeyParam "id"
  case maybeStepKey of
    Nothing -> home
    Just stepKey -> do
      runPersist $ delete stepKey
      home

stepKeyParam :: MonadSnap m => ByteString -> m (Maybe (Key Step))
stepKeyParam name = fmap (fmap mkKeyBS) (getParam name)
