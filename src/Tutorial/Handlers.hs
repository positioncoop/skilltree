{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Tutorial.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Extras.JSON
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


routes :: [(Text, AppHandler ())]
routes = [("", ifTop indexH)
         ,("new", ifTop $ authorize newH)
         ,(":id", do
              tentity <- requestedTutorial
              route [("", ifTop $ showH tentity)
                    ,("", authorize $ route [("edit", ifTop $ editH tentity)
                                            ,("delete", ifTop $ deleteH tentity)
                                            ,("move", ifTop $ moveH tentity)
                                            ,("steps", route (Step.Handlers.routes tentity))
                                            ])])]

requestedTutorial :: AppHandler TutorialEntity
requestedTutorial = do
  tutorialKey <- getParam "id"
  tutorial <- require $ runPersist $ get tutorialKey
  return $ Entity tutorialKey tutorial

authorize :: AppHandler () -> AppHandler ()
authorize = requireUser auth (redirect "/auth/login")

home :: AppHandler ()
home = redirect "/"

indexH :: AppHandler ()
indexH = do loggedIn <- with auth isLoggedIn
            tutorials <- if loggedIn
                         then lookupAllTutorials
                         else lookupPublishedTutorials
            writeJSON tutorials

showH :: TutorialEntity -> AppHandler ()
showH = renderWithSplices "tutorials/show" . Tutorial.Splices.entitySplice

newH :: AppHandler ()
newH = handleTutorialAjax (runForm "new" Tutorial.Form.newForm) (runPersist . insert)

moveH :: TutorialEntity -> AppHandler ()
moveH entity@(Entity tutorialKey _) =
  handleTutorialAjax (runForm "move" $ Tutorial.Form.moveForm entity) (runPersist . replace tutorialKey)

editH :: TutorialEntity -> AppHandler ()
editH entity@(Entity tutorialKey tutorial) = do
  response <- runMultipartForm "edit" $ Tutorial.Form.editForm tutorial
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

handleTutorialAjax :: AppHandler (View Text, Maybe Tutorial) -> (Tutorial -> AppHandler a) -> AppHandler ()
handleTutorialAjax form doWithResult =  do
  response <- form
  case response of
    (_, Nothing) -> return ()
    (_, Just tutorial') -> do
      void $ doWithResult tutorial'
      return ()
