{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Snap.Plus.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Auth
import Database.Persist
import Database.Persist.Sql
import Snap.Snaplet.Persistent

import Application

authorize :: AppHandler t -> AppHandler t
authorize = requireUser auth (redirect "/auth/login")

data Resource record = (PersistEntity record, PersistEntityBackend record ~ SqlBackend) =>
  Resource {indexHandlerField :: AppHandler ()
           ,newHandlerField :: AppHandler ()
           ,showHandlerField :: Entity record -> AppHandler ()
           ,editHandlerField :: Entity record -> AppHandler ()
           ,deleteHandlerField :: Entity record -> AppHandler ()}

replaceEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
              => Key record -> record -> AppHandler ()
replaceEntity key = runPersist . (replace key)

deleteEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
             => Key record -> AppHandler ()
deleteEntity = runPersist . delete

resourceRoutes :: Resource record -> [(Text, AppHandler ())]
resourceRoutes (Resource indexHandler newHandler showHandler editHandler deleteHandler) =
  [("", ifTop indexHandler)
  ,("new", newHandler)
  ,(":id", do
       entity <- requestedEntity
       route [("", ifTop $ showHandler entity)
             ,("edit", editHandler entity)
             ,("delete", deleteHandler entity)
             ])]

requestedEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend) =>
                   AppHandler (Entity record)
requestedEntity = do
  key <- getParam "id"
  entity <- require $ runPersist $ get key
  return $ Entity key entity

home :: AppHandler ()
home = redirect "/"
