{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Snap.Plus.Paths where

import Database.Persist.Types
import Snap.Snaplet.Persistent (showKey)
import Prelude hiding ((++))
import Snap.Plus

class Paths p where
  indexPath :: Entity p -> Text

showPath :: Paths p => Entity p -> Text
showPath e@(Entity key _) = (indexPath e) ++ showKey key

editPath :: Paths p => Entity p -> Text
editPath p = showPath p ++ "/edit"

deletePath :: Paths p => Entity p -> Text
deletePath p = showPath p ++ "/delete"

newPath :: Paths p => Entity p -> Text
newPath p = indexPath p ++ "/new"
