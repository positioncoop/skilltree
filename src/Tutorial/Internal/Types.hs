{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tutorial.Internal.Types where

import           Data.Aeson.Types
import           Database.Persist.TH
import           Database.Persist.Types
import           Prelude                   hiding ((++))
import           Snap.Plus
import           Snap.Plus.Paths
import           Snap.Snaplet.Persistent   (showKey)
import           Tutorial.Internal.Publish

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
tutorialPath (Entity key _) = showPath key

tutorialEditPath :: TutorialEntity -> Text
tutorialEditPath (Entity key _) = editPath key

tutorialDeletePath :: TutorialEntity -> Text
tutorialDeletePath (Entity key _) = deletePath key

tutorialStepNewPath :: TutorialEntity -> Text
tutorialStepNewPath (Entity key _) = showPath key ++ "/steps/new"

instance Paths TutorialId Tutorial where
  indexPath _ = "/tutorials/"
