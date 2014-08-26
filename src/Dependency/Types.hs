{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Dependency.Types where

import           Database.Persist.TH
import           Database.Persist.Types
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Snaplet.Persistent (showKey)

import qualified Tutorial.Types          as T

share [mkPersist sqlSettings] [persistLowerCase|
Dependency
  tutorialId T.TutorialId
  dependencyId T.TutorialId
  deriving Show
  deriving Eq
|]

type DependencyEntity = Entity Dependency

dependencyPath :: DependencyEntity -> Text
dependencyPath (Entity key _) = "/dependencies/" ++ showKey key

dependencyDeletePath :: DependencyEntity -> Text
dependencyDeletePath entity = dependencyPath entity ++ "/delete"
