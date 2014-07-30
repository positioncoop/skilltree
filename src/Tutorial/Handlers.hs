{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Handlers where

import Prelude hiding ((++))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Aeson
import Snap (liftIO)
import Snap.Core hiding (redirect)
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Extras.JSON
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Tutorial.Form
import Tutorial.Types
import Tutorial.Splices
import qualified Step.Handlers
import Helpers
import Forms

import Application

routes :: [(ByteString, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop newH)
         , (":id", tutorialHandler)
         ]

tutorialHandler :: AppHandler ()
tutorialHandler = do
  maybeTutorialKey <- tutorialKeyParam "id"
  case maybeTutorialKey of
    Nothing -> pass
    Just tutorialKey -> do
      maybeTutorial <- runPersist $ get tutorialKey
      case maybeTutorial of
        Nothing -> pass
        Just tutorial -> do
          let tentity = Entity tutorialKey tutorial
          route [("", ifTop $ showH tentity)
                ,("edit", ifTop $ editH tentity)
                ,("delete", ifTop $ deleteH tentity)
                ,("steps", route (Step.Handlers.routes tentity))
                ]

home :: AppHandler ()
home = redirect "/"

indexH :: AppHandler ()
indexH = do
  tutorials <- runPersist $ selectList [] [] :: AppHandler [Entity Tutorial]
  writeJSON tutorials

showH :: TutorialEntity -> AppHandler ()
showH = undefined


newH :: AppHandler ()
newH = do
  response <- runForm "new" Tutorial.Form.newForm
  case response of
    (v, Nothing) -> renderWithSplices "tutorials/form" (digestiveSplices v)
    (_, Just tutorial) -> do
      runPersist $ insert tutorial
      home

editH :: TutorialEntity -> AppHandler ()
editH entity@(Entity tutorialKey tutorial) = do
  response <- runMultipartForm "edit-tutorial" (Tutorial.Form.editForm $ tutorial)
  case response of
    (v, Nothing) -> renderWithSplices "tutorials/form" $ do Tutorial.Splices.entitySplice entity
                                                            digestiveSplices v
    (_, Just _tutorial) -> do
      runPersist $ replace tutorialKey _tutorial
      redirect $ tutorialEditPath entity

deleteH :: TutorialEntity -> AppHandler ()
deleteH (Entity tutorialKey _) = do
  runPersist $ delete tutorialKey
  home

tutorialKeyParam :: MonadSnap m => ByteString -> m (Maybe (Key Tutorial))
tutorialKeyParam name = fmap (fmap mkKeyBS) (getParam name)
