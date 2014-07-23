{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Form where

import Prelude hiding ((++))
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive
import Data.Maybe
import Forms

import Tutorial.Types
import Application

form :: Maybe Tutorial -> Form Text AppHandler NewTutorial
form mTutorial = checkM "Tutorial overlaps with existing tutorial" overlapping $
  Tutorial' <$> pure Nothing
            <*> "x" .: stringRead "Must be a number" (tutorialX <$> mTutorial)
            <*> "y" .: stringRead "Must be a number" (tutorialY <$> mTutorial)
            <*> "title" .: text (tutorialTitle <$> mTutorial)
 where overlapping (Tutorial' _ x y _) =
         isNothing <$> getTutorialAtCoords x y
