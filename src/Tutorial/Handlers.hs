{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Handlers where

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

import Tutorial.Splices
import Tutorial.Form
import Tutorial.Types
import Helpers
import Forms

import Application

resource :: Resource
resource = Resource "tutorial" "/tutorials" [] []

crud :: [(CRUD, AppHandler ())]
crud =  [ (RIndex, indexH)
        , (RShow, showH)
        , (RNew, newH)
        , (RCreate, newH)
        , (REdit, editH)
        , (RUpdate, editH)
        ]

home :: AppHandler ()
home = redirect $ T.encodeUtf8 $ "/"

indexH :: AppHandler ()
indexH = do
  tutorials <- runPersist $ selectList [] [] :: AppHandler [Entity Tutorial]
  writeJSON tutorials

showH :: AppHandler ()
showH = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> pass
    Just tutorialKey -> do
      maybeTutorial <- runPersist $ get tutorialKey
      case maybeTutorial of
        Nothing -> pass
        Just tutorial -> renderWithSplices "/tutorials/show" $ tutorialSplice tutorial

newH :: AppHandler ()
newH = do
  response <- runForm "new" Tutorial.Form.newForm
  case response of
    (v, Nothing) -> renderWithSplices "tutorials/form" (digestiveSplices v)
    (_, Just tutorial) -> do
      runPersist $ insert tutorial
      home

editH :: AppHandler ()
editH = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> pass
    Just tutorialKey -> do
      maybeTutorial <- runPersist $ get tutorialKey
      case maybeTutorial of
        Nothing -> pass
        Just tutorial -> do
          response <- runMultipartForm "edit-tutorial" (Tutorial.Form.editForm $ tutorial)
          case response of
            (v, Nothing) -> renderWithSplices "tutorials/form" (digestiveSplices v)
            (_, Just _tutorial) -> do
              runPersist $ replace tutorialKey _tutorial
              home

deleteH :: AppHandler ()
deleteH = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> home
    Just tutorialKey -> do
      runPersist $ delete tutorialKey
      home

tutorialKeyParam :: MonadSnap m => ByteString -> m (Maybe (Key Tutorial))
tutorialKeyParam name = fmap (fmap mkKeyBS) (getParam name)
