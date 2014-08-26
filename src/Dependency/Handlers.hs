{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Dependency.Handlers where

import           Data.Aeson
import           Database.Persist
import           Prelude                 hiding ((++))
import           Snap.Extras.CoreUtils
import           Snap.Extras.JSON
import           Snap.Plus
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Text.Digestive.Snap     (runForm)

import           Dependency.Form
import           Dependency.Queries
import           Dependency.Types
import           Tutorial.Types

import           Application

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: [(Text, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop $ requireUser auth authCheck newH)
         , (":id/delete", requireUser auth authCheck deleteH)
         ]

indexH :: AppHandler ()
indexH = format [JSON] $ do
  loggedIn <- with auth isLoggedIn
  dependencies <- map toLine <$> if loggedIn
                         then lookupAllDependencyPairs
                         else lookupPublishedDependencyPairs
  writeJSON dependencies
  where
    toLine (target, Entity key _, source) =
      object ["id" .= showKey key, "target" .= target, "source" .= source]

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
  dependencyKey <- requireParam "id"
  runPersist $ delete (dependencyKey :: Key Dependency)
  redirectReferer
