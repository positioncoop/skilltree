{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Tutorial.Internal.Types where

import Prelude hiding ((++))
import Snap.Plus
import Database.Persist.Types
import Database.Persist.TH
import Data.Aeson.Types
import Snap.Snaplet.Persistent (showKey)
import Tutorial.Internal.Publish

share [mkPersist sqlSettings] [persistLowerCase|
Tutorial
  x Int
  y Int
  title Text
  iconPath FilePath Maybe
  publish Publish
  deriving Show
  deriving Eq
|]


type TutorialEntity = Entity Tutorial

instance ToJSON (Entity Tutorial) where
  toJSON (Entity key (Tutorial x y title iconPath publish)) =
    object ["id" .= showKey key, "x" .= x, "y" .= y,
            "title" .= title, "iconPath" .= iconPath, "publish" .= tshow publish]

tutorialPath :: TutorialEntity -> Text
tutorialPath (Entity key _) = "/tutorials/" ++ showKey key

tutorialEditPath :: TutorialEntity -> Text
tutorialEditPath entity = tutorialPath entity ++ "/edit"

tutorialDeletePath :: TutorialEntity -> Text
tutorialDeletePath entity = tutorialPath entity ++ "/delete"

tutorialStepNewPath :: TutorialEntity -> Text
tutorialStepNewPath entity = tutorialPath entity ++ "/steps/new"
