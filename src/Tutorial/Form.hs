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

untitledTutorial :: Int -> Int -> Tutorial
untitledTutorial a b = Tutorial a b "Untitled"

newForm :: Form Text AppHandler Tutorial
newForm = checkM "Tutorial overlaps with existing tutorial" overlapping $
  untitledTutorial <$> "x" .: stringRead "Must be a number" Nothing
                   <*> "y" .: stringRead "Must be a number" Nothing
  where overlapping (Tutorial x y _) = do result <- runPersist (selectList [TutorialX ==. x, TutorialY ==. y] [LimitTo 1])
                                          return (result == [])

editForm :: Tutorial -> Form Text AppHandler Tutorial
editForm (Tutorial x y title) = Tutorial x y <$> "title" .: text (Just title)
