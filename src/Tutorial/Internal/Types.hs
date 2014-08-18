{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Tutorial.Internal.Types where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Plus.Paths
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

-- TODO(mjr 2014-08-18): -- fixme inline these
tutorialPath :: TutorialEntity -> Text
tutorialPath = showPath

tutorialEditPath :: TutorialEntity -> Text
tutorialEditPath = editPath

tutorialDeletePath :: TutorialEntity -> Text
tutorialDeletePath = deletePath

tutorialStepNewPath :: TutorialEntity -> Text
tutorialStepNewPath = (++ "/steps/new") . showPath

instance Paths Tutorial where
  indexPath _ = "/tutorials/"
