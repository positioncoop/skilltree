{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module TutorialWeek.Types where

import Prelude hiding ((++))
import Snap.Plus
import Database.Persist.Types
import Database.Persist.TH
import Snap.Snaplet.Persistent (showKey)
import Data.Aeson.Types

import qualified Tutorial.Types as T
import qualified Week.Types as W

share [mkPersist sqlSettings] [persistLowerCase|
TutorialWeek
  tutorialId T.TutorialId
  weekId W.WeekId
  deriving Show
  deriving Eq
|]

type WeekEntity = Entity TutorialWeek
