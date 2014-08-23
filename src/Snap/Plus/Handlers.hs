{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Snap.Plus.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Control.Arrow
import Snap.Snaplet.Auth
import Database.Persist
import Database.Persist.Sql
import Snap.Snaplet.Persistent

import Application

authorize :: AppHandler t -> AppHandler t
authorize = requireUser auth (redirect "/auth/login")

data Resource record = (PersistEntity record, PersistEntityBackend record ~ SqlBackend) =>
  SimpleResource {indexHandlerField :: AppHandler ()
                 ,newHandlerField :: AppHandler ()
                 ,showHandlerField :: Entity record -> AppHandler ()
                 ,editHandlerField :: Entity record -> AppHandler ()
                 ,deleteHandlerField :: Entity record -> AppHandler ()} |
  CompoundResource {basicRoutes :: Resource record
                   ,moreMemberRoutes :: [(Text, Entity record -> AppHandler ())]
                                        {-nested, member, new, collection-}
                   }

replaceEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
              => Key record -> record -> AppHandler ()
replaceEntity key = runPersist . (replace key)

deleteEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
             => Key record -> AppHandler ()
deleteEntity = runPersist . delete

routeResource :: Resource record -> AppHandler ()
routeResource = route . resourceRoutes

resourceRoutes :: Resource record -> [(Text, AppHandler ())]
resourceRoutes (SimpleResource indexHandler newHandler showHandler editHandler deleteHandler) =
  [("", ifTop indexHandler)
  ,("new", newHandler)
  ,(":id", do
       entity <- requestedEntity
       route [("", ifTop $ showHandler entity)
             ,("edit", editHandler entity)
             ,("delete", deleteHandler entity)
             ])]
resourceRoutes (CompoundResource
                 (SimpleResource indexHandler newHandler showHandler editHandler deleteHandler)
                 members) =
  [("", ifTop indexHandler)
  ,("new", newHandler)
  ,(":id", routeWithEntity memberRoutes =<< requestedEntity)]
  where routeWithEntity partialRoutes entity = route $ map (withEntity entity) partialRoutes
        withEntity entity (pathPart, handler) = (pathPart, handler entity)
        memberRoutes = [("", ifTop . showHandler)
                       ,("edit", editHandler)
                       ,("delete", deleteHandler)
                       ] ++ members

requestedEntity :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend) =>
                   AppHandler (Entity record)
requestedEntity = do
  key <- requireParam "id"
  entity <- require $ runPersist $ get key
  return $ Entity key entity

home :: AppHandler ()
home = redirect "/"
