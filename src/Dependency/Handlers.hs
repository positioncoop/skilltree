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
import Snap.Extras.JSON
import Data.Aeson
import Database.Persist (insert)
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Dependency.Form
import Dependency.Types

import Application

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: [(Text, AppHandler ())]
routes = [ ("new", ifTop $ requireUser auth authCheck newH)
         ]

newH :: AppHandler ()
newH = do
  response <- runForm "new" Dependency.Form.newForm
  case response of
    (_, Nothing) -> return ()
    (_, Just dependency) -> do
      void $ runPersist $ insert dependency
      return ()
