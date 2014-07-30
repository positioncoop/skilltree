{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Step.Splices where

import Prelude hiding ((++))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted
import Snap

import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P

import Step.Types
import Helpers
import Application

entitySplice :: StepEntity -> Splices (Splice AppHandler)
entitySplice entity@(Entity _id (Step _tutorialId _content _ordinal)) = do
  "stepId" ## textSplice $ P.showKey _id
  "stepTutorialId" ## textSplice $ tshow _tutorialId
  "stepContent" ## textSplice _content
  "stepOrdinal" ## textSplice $ tshow _ordinal
  "stepEditPath" ## textSplice $ stepEditPath entity
