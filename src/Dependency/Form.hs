{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Dependency.Form where

import Control.Lens
import Prelude hiding ((++))
import Text.Digestive
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified System.IO as IO
import qualified Control.Monad.Trans.Resource as R
import Control.Monad.Trans.Either (runEitherT, left, right)
import Data.Conduit.ImageSize

import Dependency.Types
import Application

newForm :: Form Text AppHandler Dependency
newForm = checkM "Dependency invalid" validateDep $
  Dependency <$> "tutorialId" .: keyForm Nothing
             <*> "dependencyId" .: keyForm Nothing
  where
    validateDep (Dependency tutorialKey dependencyKey) | tutorialKey == dependencyKey = return False
    validateDep (Dependency tutorialKey dependencyKey) = do result <- runPersist (selectList [DependencyTutorialId ==. tutorialKey,
                                                                                              DependencyDependencyId ==. dependencyKey]
                                                                                  [LimitTo 1])
                                                            return (result == [])
    keyForm def = validate (\s -> case readSafe s of
                                    Nothing -> Error "Not a number"
                                    Just i -> Success (mkKey i)) (text def)
