{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Form where

import Control.Lens
import Prelude hiding ((++))
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive
import Snap
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore

import Tutorial.Types
import Application

untitledTutorial :: Int -> Int -> Tutorial
untitledTutorial a b = Tutorial a b "Untitled" Nothing

newForm :: Form Text AppHandler Tutorial
newForm = checkM "Tutorial overlaps with existing tutorial" overlapping $
  untitledTutorial <$> "x" .: stringRead "Must be a number" Nothing
                   <*> "y" .: stringRead "Must be a number" Nothing
  where overlapping (Tutorial x y _ _) = do result <- runPersist (selectList [TutorialX ==. x, TutorialY ==. y] [LimitTo 1])
                                            return (result == [])

editForm :: Tutorial -> Form Text AppHandler Tutorial
editForm (Tutorial x y title mIconPath) = validateM mkMedia $ Tutorial x y <$> "title" .: text (Just title) <*> "iconPath" .: file
    where mkMedia :: Tutorial -> AppHandler (Result Text Tutorial)
          mkMedia (Tutorial a b c Nothing) = return $ Success $ Tutorial a b c mIconPath
          mkMedia (Tutorial a b c (Just _path)) = do store <- use filestore
                                                     url <- storeFile store _path Nothing
                                                     return $ Success $ Tutorial a b c (Just $ T.unpack url)
