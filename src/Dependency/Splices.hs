{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Dependency.Splices where

import           Heist
import           Heist.Interpreted
import           Prelude           hiding ((++))

import           Dependency.Types
import           Tutorial.Types

import           Application

splice :: (TutorialEntity -> Splices (Splice AppHandler)) -> (TutorialEntity, DependencyEntity) -> Splices (Splice AppHandler)
splice tutorialSplice (tutorial, dependencyEntity) = do "dependencyDeletePath" ## textSplice $ dependencyDeletePath dependencyEntity
                                                        "dependencyTutorial" ## runChildrenWith $ tutorialSplice tutorial
