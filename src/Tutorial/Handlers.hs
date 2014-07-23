{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Handlers where

import Prelude hiding ((++))
import Control.Applicative hiding (Const)
import Data.Maybe
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

import Application

resource :: Resource
resource = Resource "tutorial" "/tutorials" [] []

crud :: [(CRUD, AppHandler ())]
crud =  [ (RNew, newH)
        , (RShow, showH)
        , (REdit, editH)
        , (RUpdate, editH)
        , (RCreate, newH)
        , (RIndex, indexH)
        ]

home :: AppHandler ()
home = redirect $ T.encodeUtf8 $ rRoot resource

tutorialsHandler :: ByteString -> AppHandler ()
tutorialsHandler template =  do
  tutorials <- getAllTutorials
  renderWithSplices template (tutorialsSplice tutorials)

indexH :: AppHandler ()
indexH = do tutorials <- getAllTutorials
            writeJSON tutorials

showH :: AppHandler ()
showH = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> pass
    Just tutorialKey -> do
      maybeTutorial <- getTutorialById tutorialKey
      case maybeTutorial of
        Nothing -> pass
        Just tutorial -> renderWithSplices "/tutorials/show" $ tutorialSplice tutorial

newH :: AppHandler ()
newH = do
  response <- runForm "new" (Tutorial.Form.form Nothing)
  case response of
    (v, Nothing) -> renderWithSplices "tutorials/form" (digestiveSplices v)
    (_, Just tutorial) -> do
      insertTutorial tutorial
      home

editH :: AppHandler ()
editH = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> home
    Just tutorialKey -> do
      maybeTutorial <- getTutorialById tutorialKey
      response <- runForm "edit-tutorial" (Tutorial.Form.form maybeTutorial)
      case response of
        (v, Nothing) -> renderWithSplices "tutorials/form" (digestiveSplices v)
        (_, Just e) -> do
          updateTutorial e { tutorialId = tutorialKey }
          home

deleteH :: AppHandler ()
deleteH = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> home
    Just tutorialKey -> do
      deleteTutorialById tutorialKey
      home

tutorialKeyParam :: MonadSnap m => ByteString -> m (Maybe Int)
tutorialKeyParam name = (>>= breadSafe) <$> (getParam name)
