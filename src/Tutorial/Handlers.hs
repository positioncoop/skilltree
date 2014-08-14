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

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: [(Text, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop $ requireUser auth authCheck newH)
         , (":id", tutorialHandler)
         ]

tutorialHandler :: AppHandler ()
tutorialHandler = do
  tutorialKey <- getParam "id"
  tutorial <- require $ runPersist $ get tutorialKey
  let tentity = Entity tutorialKey tutorial
  route [("", ifTop $ showH tentity)
        ,("", requireUser auth authCheck $
              route [("edit", ifTop $ editH tentity)
                    ,("delete", ifTop $ deleteH tentity)
                    ,("move", ifTop $ moveH tentity)
                    ,("publish", ifTop $ publishH tentity)
                    ,("steps", route (Step.Handlers.routes tentity))
                    ])]

home :: AppHandler ()
home = redirect "/"

indexH :: AppHandler ()
indexH = writeJSON =<< (runPersist $ selectList [] [] :: AppHandler [Entity Tutorial])

showH :: TutorialEntity -> AppHandler ()
showH = renderWithSplices "tutorials/show" . Tutorial.Splices.entitySplice

newH :: AppHandler ()
newH = handleTutorialAjax (runForm "new" Tutorial.Form.newForm) (runPersist . insert)

moveH :: TutorialEntity -> AppHandler ()
moveH entity@(Entity tutorialKey _) =
  handleTutorialAjax (runForm "move" $ Tutorial.Form.moveForm entity) (runPersist . replace tutorialKey)

publishH :: TutorialEntity -> AppHandler ()
publishH entity@(Entity tutorialKey _) =
  handleTutorialAjax (runForm "publish" $ Tutorial.Form.publishForm entity) (runPersist . replace tutorialKey)

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
