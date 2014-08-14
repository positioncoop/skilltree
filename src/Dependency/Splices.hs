{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Dependency.Splices where

import Prelude hiding ((++))
import Heist
import Heist.Interpreted

import Dependency.Types
import Tutorial.Types

import Application

splice :: (TutorialEntity -> Splices (Splice AppHandler)) -> (TutorialEntity, DependencyEntity) -> Splices (Splice AppHandler)
splice tutorialSplice (tutorial, dependencyEntity) = do "dependencyDeletePath" ## textSplice $ dependencyDeletePath dependencyEntity
                                                        "dependencyTutorial" ## runChildrenWith $ tutorialSplice tutorial
