{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Tutorial.Types where

import Prelude hiding ((++))
import Data.Text (Text)
import Database.Persist.Types
import Database.Persist.TH
import qualified Snap.Snaplet.Persistent as P
import Database.Persist
import Data.Aeson.Types
import Snap.Snaplet.Persistent (showKey)

import Application

share [mkPersist sqlSettings] [persistLowerCase|
Tutorial
  x Int
  y Int
  title Text
  iconPath FilePath Maybe
  deriving Show
  deriving Eq
|]

type TutorialEntity = Entity Tutorial

instance ToJSON (Entity Tutorial) where
  toJSON (Entity id (Tutorial x y title iconPath)) =
    object ["id" .= showKey id, "x" .= x, "y" .= y, "title" .= title, "iconPath" .= iconPath]

tutorialPath :: TutorialEntity -> Text
tutorialPath (Entity key _) = "/tutorials/" ++ showKey key

tutorialEditPath :: TutorialEntity -> Text
tutorialEditPath entity = tutorialPath entity ++ "/edit"

tutorialDeletePath :: TutorialEntity -> Text
tutorialDeletePath entity = tutorialPath entity ++ "/delete"

tutorialStepNewPath :: TutorialEntity -> Text
tutorialStepNewPath entity = tutorialPath entity ++ "/steps/new"
