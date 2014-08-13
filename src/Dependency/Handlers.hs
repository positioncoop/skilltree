{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Dependency.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Extras.CoreUtils
import Snap.Extras.JSON
import Data.Aeson
import Database.Persist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Dependency.Form
import Dependency.Types

import Application

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: [(Text, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop $ requireUser auth authCheck newH)
         , (":id/delete", requireUser auth authCheck deleteH)
         ]

indexH :: AppHandler ()
indexH = undefined

newH :: AppHandler ()
newH = do
  response <- runForm "new" Dependency.Form.newForm
  case response of
    (_, Nothing) -> return ()
    (_, Just dependency) -> do
      void $ runPersist $ insert dependency
      return ()

deleteH :: AppHandler ()
deleteH = do
  dependencyKey <- getParam "id"
  runPersist $ delete (dependencyKey :: Key Dependency)
  redirectReferer
