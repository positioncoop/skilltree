{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Snap.Plus.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Auth
import Database.Persist
import Snap.Snaplet.Persistent

-- TODO(mjr 2014-08-18): fixme don't depend on Skilltree types
import Tutorial.Types
import Application

authorize :: AppHandler t -> AppHandler t
authorize = requireUser auth (redirect "/auth/login")

-- TODO(mjr 2014-08-18): fixme index should take a list of tutorials(?)
data R = R {indexHandlerField :: AppHandler ()
           ,newHandlerField :: AppHandler ()
           ,showHandlerField :: Entity Tutorial -> AppHandler ()
           ,editHandlerField :: Entity Tutorial -> AppHandler ()
           ,deleteHandlerField :: Entity Tutorial -> AppHandler ()}

resourceRoutes :: R -> [(Text, AppHandler ())]
resourceRoutes (R indexHandler newHandler showHandler editHandler deleteHandler) =
  [("", ifTop indexHandler)
  ,("new", newHandler)
  ,(":id", do
       tentity <- requestedTutorial
       route [("", ifTop $ showHandler tentity)
             ,("edit", editHandler tentity)
             ,("delete", deleteHandler tentity)
             ])]

requestedTutorial :: AppHandler TutorialEntity
requestedTutorial = do
  tutorialKey <- getParam "id"
  tutorial <- require $ runPersist $ get tutorialKey
  return $ Entity tutorialKey tutorial

home :: AppHandler ()
home = redirect "/"
