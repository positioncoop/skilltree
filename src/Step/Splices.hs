{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Step.Splices where

import           Data.Text               (Text)
import qualified Data.Text               as T
import           Heist
import           Heist.Interpreted
import           Prelude                 hiding ((++))
import           Snap.Plus

import           Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P

import           Application
import           Step.Types

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
                              YouTube -> "//www.youtube-nocookie.com/embed/" ++ code ++ "?rel=0&autoplay=1"
                              Vimeo -> "//player.vimeo.com/video/" ++ code ++ "?autoplay=1"
