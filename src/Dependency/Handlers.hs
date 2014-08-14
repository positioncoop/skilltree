{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Dependency.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Extras.CoreUtils
import Snap.Extras.JSON
import Data.Aeson
import Database.Persist
import Text.Digestive.Snap (runForm)

import Dependency.Form
import Dependency.Types
import Tutorial.Types
import Dependency.Queries

import Application

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: [(Text, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop $ requireUser auth authCheck newH)
         , (":id/delete", requireUser auth authCheck deleteH)
         ]

indexH :: AppHandler ()
indexH = do
  loggedIn <- with auth isLoggedIn
  dependencies <- map toLine <$> if loggedIn
                         then lookupAllDependencyPairs
                         else lookupPublishedDependencyPairs
  writeJSON dependencies
  where
    toPoint (Tutorial x y _ _ _) = object ["x" .= x, "y" .= y]
    toLine (Entity _ target, Entity _ source) =
      object ["target" .= toPoint target, "source" .= toPoint source]

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
