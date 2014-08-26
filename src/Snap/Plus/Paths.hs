{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Snap.Plus.Paths where

import           Database.Persist.Sql
import           Database.Persist.Types
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Snaplet.Persistent (showKey)

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
