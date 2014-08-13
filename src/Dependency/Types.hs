{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Dependency.Types where

import Prelude hiding ((++))
import Snap.Plus
import Database.Persist.Types
import Database.Persist.TH
import Snap.Snaplet.Persistent (showKey)

import qualified Tutorial.Types as T

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
