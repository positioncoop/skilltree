{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Dependency.Form where

import Prelude hiding ((++))
import Text.Digestive
import Snap.Plus
import Snap.Snaplet.Persistent
import Database.Persist

import Dependency.Types
import Application

newForm :: Form Text AppHandler Dependency
newForm = checkM "Dependency invalid" validateDep $
  Dependency <$> "tutorialId" .: keyForm Nothing
             <*> "dependencyId" .: keyForm Nothing
  where
    validateDep (Dependency tutorialKey dependencyKey) | tutorialKey == dependencyKey = return False
    validateDep (Dependency tutorialKey dependencyKey) =
      do result <- runPersist (selectList [DependencyTutorialId ==. tutorialKey, DependencyDependencyId ==. dependencyKey]
                                          [LimitTo 1])
         return (null result)
    keyForm def = validate (\s -> case readSafe s of
                                    Nothing -> Error "Not a number"
                                    Just i -> Success (mkKey i)) (text def)
