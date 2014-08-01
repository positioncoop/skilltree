{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Handlers where

import Prelude hiding ((++))
import Control.Monad (when)
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
import SnapPrelude
import Forms

import Tutorial.Form
import Tutorial.Types
import Tutorial.Splices
import qualified Step.Handlers
import Tutorial.Queries

import Application

routes :: [(ByteString, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop newH)
         , (":id", tutorialHandler)
         ]

tutorialHandler :: AppHandler ()
tutorialHandler = do
  tutorialKey <- getParam' "id"
  tutorial <- require $ runPersist $ get tutorialKey
  let tentity = Entity tutorialKey tutorial
  route [("", ifTop $ showH tentity)
        ,("edit", ifTop $ editH tentity)
        ,("delete", ifTop $ deleteH tentity)
        ,("move", ifTop $ moveH tentity)
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
    (_, Nothing) -> return ()
    (_, Just tutorial) -> do
      void $ runPersist $ insert tutorial
      return ()

moveH :: TutorialEntity -> AppHandler ()
moveH entity@(Entity tutorialKey _) = do
  response <- runForm "move" (Tutorial.Form.moveForm entity)
  case response of
    (_, Nothing) -> return ()
    (_, Just _tutorial) -> do
      runPersist $ replace tutorialKey _tutorial
      return ()

editH :: TutorialEntity -> AppHandler ()
editH entity@(Entity tutorialKey tutorial) = do
  response <- runMultipartForm "edit" (Tutorial.Form.editForm tutorial)
  case response of
    (v, Nothing) -> renderWithSplices "tutorials/form" $ do Tutorial.Splices.entitySplice entity
                                                            digestiveSplices v
    (_, Just _tutorial) -> do
      runPersist $ replace tutorialKey _tutorial
      redirect $ tutorialEditPath entity

deleteH :: TutorialEntity -> AppHandler ()
deleteH entity@(Entity tutorialKey _) = do
  steps <- lookupTutorialSteps entity
  if null steps
    then do runPersist $ delete tutorialKey
            home
    else redirect $ tutorialEditPath entity

