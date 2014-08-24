{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Tutorial.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Plus.Handlers
import Snap.Plus.Forms
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Extras.JSON
import Data.Aeson
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist
import Text.Digestive.View

import Tutorial.Form
import Tutorial.Types
import Tutorial.Splices
import qualified Step.Handlers
import Tutorial.Queries

import Application

tutorialResource :: Resource Tutorial
tutorialResource = Resource indexH (authorize newH) showH (authorize . editH) (authorize . deleteH)
                   [("move", authorize . moveH)
                   ,("steps", authorize . routeResource . Step.Handlers.nestedStepResource)]
                   [("admin", authorize $ render "tutorials/admin")]

routes :: [(Text, AppHandler ())]
routes = resourceRoutes tutorialResource

indexH :: AppHandler ()
indexH = routeFormats [([JSON], indexJsonH)
                      ,([HTML, NotSpecified], indexHtmlH)]

indexJsonH :: AppHandler ()
indexJsonH = do loggedIn <- with auth isLoggedIn
                tutorials <- if loggedIn then lookupAllTutorials else lookupPublishedTutorials
                writeJSON tutorials

indexHtmlH :: AppHandler ()
indexHtmlH = render "tutorials/index"

showH :: TutorialEntity -> AppHandler ()
showH = renderWithSplices "tutorials/show" . Tutorial.Splices.entitySplice

newH :: AppHandler ()
newH = route [("", method POST newPost), ("", method GET newGet)]
  where newGet = render "tutorials/new"
        newPost = handleTutorialAjax (runForm "new" Tutorial.Form.newForm) (insertTutorial)
        insertTutorial record = do k <- runPersist $ insert record
                                   return $ Entity k record

moveH :: TutorialEntity -> AppHandler ()
moveH entity@(Entity tutorialKey _) =
  handleTutorialAjax (runForm "move" $ Tutorial.Form.moveForm entity) (runPersist . replace tutorialKey)

editH :: TutorialEntity -> AppHandler ()
editH entity@(Entity tutorialKey tutorial) = do
  response <- runMultipartForm "edit" $ Tutorial.Form.editForm tutorial
  case response of
    (v, Nothing) -> renderWithSplices "tutorials/edit" $ do Tutorial.Splices.entitySplice entity
                                                            digestiveSplices v
    (_, Just _tutorial) -> do
      runPersist $ replace tutorialKey _tutorial
      redirect $ tutorialEditPath entity

deleteH :: TutorialEntity -> AppHandler ()
deleteH entity@(Entity tutorialKey _) = do
  steps <- lookupTutorialSteps entity
  dependencies <- lookupTutorialDependencies entity
  if null steps && null dependencies
    then do runPersist $ delete tutorialKey
            home
    else redirectReferer

handleTutorialAjax :: ToJSON a => AppHandler (View Text, Maybe Tutorial) -> (Tutorial -> AppHandler a) -> AppHandler ()
handleTutorialAjax form doWithResult =  do
  response <- form
  case response of
    (_, Nothing) -> return ()
    (_, Just tutorial') -> do
      writeJSON =<< doWithResult tutorial'
