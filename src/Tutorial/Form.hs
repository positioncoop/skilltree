{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Form where

import Prelude hiding ((++))
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive
import Snap.Snaplet.Persistent
import Database.Persist
import Forms

import Tutorial.Types
import Application

form :: Maybe Tutorial -> Form Text AppHandler Tutorial
form mTutorial = checkM "Tutorial overlaps with existing tutorial" overlapping $
  Tutorial <$> "x" .: stringRead "Must be a number" (tutorialX <$> mTutorial)
           <*> "y" .: stringRead "Must be a number" (tutorialY <$> mTutorial)
           <*> "title" .: text (tutorialTitle <$> mTutorial)
 where overlapping (Tutorial x y _) = do result <- runPersist (selectList [TutorialX ==. x, TutorialY ==. y] [LimitTo 1])
                                         return (result == [])
