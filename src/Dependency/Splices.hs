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

splice :: (TutorialEntity -> Splices (Splice AppHandler)) -> (DependencyEntity, TutorialEntity) -> Splices (Splice AppHandler)
splice tutorialSplice (dependencyEntity, tutorial) = do "dependencyDeletePath" ## textSplice $ dependencyDeletePath dependencyEntity
                                                        "dependencyTutorial" ## runChildrenWith $ tutorialSplice tutorial
