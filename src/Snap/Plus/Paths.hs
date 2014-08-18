{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls,
             MultiParamTypeClasses #-}

module Snap.Plus.Paths where

import Database.Persist.Types
import Database.Persist.Sql
import Snap.Snaplet.Persistent (showKey)
import Prelude hiding ((++))
import Snap.Plus

class KeyBackend (PersistEntityBackend record) record ~ k => Paths k record where
  indexPath :: k -> Text

showPath :: Paths k e => k -> Text
showPath k = (indexPath k) ++ showKey k

editPath :: Paths k e => k -> Text
editPath k = showPath k ++ "/edit"

deletePath :: Paths k e => k -> Text
deletePath k = showPath k ++ "/delete"

newPath :: Paths k e => k -> Text
newPath k = indexPath k ++ "/new"
