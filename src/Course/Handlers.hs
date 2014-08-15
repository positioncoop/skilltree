{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Course.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Extras.CoreUtils
import Snap.Extras.JSON
import Data.Aeson
import Database.Persist
import Text.Digestive.Snap (runForm)

import Course.Form
import Course.Types

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
  courses <- runPersist $ selectList [] [] :: AppHandler [CourseEntity]
  writeJSON courses

newH :: AppHandler ()
newH = do
  response <- runForm "new" Course.Form.newForm
  case response of
    (_, Nothing) -> redirect "/"
    (_, Just course) -> do
      void $ runPersist $ insert course
      redirect "/"

deleteH :: AppHandler ()
deleteH = do
  courseKey <- getParam "id"
  runPersist $ delete (courseKey :: Key Course)
  redirectReferer
