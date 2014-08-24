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

data Resource record = (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
                     => Resource {indexHandlerField :: AppHandler ()
                                 ,newHandlerField :: AppHandler ()
                                 ,showHandlerField :: Entity record -> AppHandler ()
                                 ,editHandlerField :: Entity record -> AppHandler ()
                                 ,deleteHandlerField :: Entity record -> AppHandler ()
                                 ,moreMemberRoutes :: [(Text, Entity record -> AppHandler ())]
                                 ,moreCollectionRoutes :: [(Text, AppHandler ())]}

replaceEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
              => Key record -> record -> AppHandler ()
replaceEntity key = runPersist . (replace key)

deleteEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
             => Key record -> AppHandler ()
deleteEntity = runPersist . delete

routeResource :: Resource record -> AppHandler ()
routeResource = route . resourceRoutes

resourceRoutes :: Resource record -> [(Text, AppHandler ())]
resourceRoutes (Resource indexHandler newHandler showHandler editHandler deleteHandler member collection) =
   (("", ifTop indexHandler) : collection)
   ++ [("new", newHandler)]
   ++ [(":id", do entity <- requestedEntity
                  routeWithEntity entity $
                    [("", ifTop . showHandler)
                    ,("edit", editHandler)
                    ,("delete", deleteHandler)]
                    ++ member)]
  where
    routeWithEntity entity = route . map (withEntity entity)
    withEntity entity (pathPart, handler) = (pathPart, handler entity)

requestedEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend) =>
                   AppHandler (Entity record)
requestedEntity = do
  key <- requireParam "id"
  entity <- require $ runPersist $ get key
  return $ Entity key entity

home :: AppHandler ()
home = redirect "/"
