{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Dependency.Form where

import           Database.Persist
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Snaplet.Persistent
import           Text.Digestive

import           Application
import           Dependency.Types

newForm :: Form Text AppHandler Dependency
newForm = checkM "Dependency invalid" validateDep $
  Dependency <$> "tutorialId" .: keyForm Nothing
             <*> "dependencyId" .: keyForm Nothing
  where
    validateDep (Dependency tutorialKey dependencyKey) | tutorialKey == dependencyKey = return False
    validateDep (Dependency tutorialKey dependencyKey) =
      do result <- (++) <$> runPersist (selectList [DependencyTutorialId ==. tutorialKey, DependencyDependencyId ==. dependencyKey]
                                          [LimitTo 1])
                        <*> runPersist (selectList [DependencyTutorialId ==. dependencyKey, DependencyDependencyId ==. tutorialKey]
                                          [LimitTo 1])
         return (null result)
    keyForm def = validate (\s -> case readSafe s of
                                    Nothing -> Error "Not a number"
                                    Just i -> Success (mkKey i)) (text def)
