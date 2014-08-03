{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Step.Types where

import Prelude hiding ((++))
import Snap.Plus
import Database.Persist.Types
import Database.Persist.TH
import Snap.Snaplet.Persistent (showKey)
import Step.VideoType

share [mkPersist sqlSettings] [persistLowerCase|
Step
  tutorialId Int
  content Text
  ordinal Int
  videoCode Text Maybe
  videoProvider VideoProvider Maybe
  deriving Show
  deriving Eq
|]

type StepEntity = Entity Step


stepPath :: StepEntity -> Text
stepPath (Entity key _) = "/steps/" ++ showKey key

stepEditPath :: StepEntity -> Text
stepEditPath entity = stepPath entity ++ "/edit"

stepDeletePath :: StepEntity -> Text
stepDeletePath entity = stepPath entity ++ "/delete"
