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

import Step.VideoType
import Step.Types
import SnapPrelude
import Application

entitySplice :: StepEntity -> Splices (Splice AppHandler)
entitySplice entity@(Entity id (Step tutorialId content ordinal videoCode videoProvider)) = do
  "stepId" ## textSplice $ P.showKey id
  "stepTutorialId" ## textSplice $ tshow tutorialId
  "stepContent" ## textSplice content
  "stepOrdinal" ## textSplice $ tshow ordinal
  "stepEditPath" ## textSplice $ stepEditPath entity
  "stepDeletePath" ## textSplice $ stepDeletePath entity
  "stepVideoCode" ## textSplice $ fromMaybe "" videoCode
  "stepVideo" ## case (,) <$> videoProvider <*> videoCode of
    Nothing -> return []
    Just (provider, code) -> runChildrenWith $
      "url" ## textSplice $ case provider of
                              YouTube -> "//www.youtube-nocookie.com/embed/" ++ code ++ "?rel=0"
                              Vimeo -> "//player.vimeo.com/video/" ++ code
