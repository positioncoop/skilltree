{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TutorialWeek.Types where

import           Data.Aeson.Types
import           Database.Persist.TH
import           Database.Persist.Types
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Snaplet.Persistent (showKey)

import qualified Tutorial.Types          as T
import qualified Week.Types              as W

share [mkPersist sqlSettings] [persistLowerCase|
TutorialWeek
  tutorialId T.TutorialId
  weekId W.WeekId
  deriving Show
  deriving Eq
|]

type TutorialWeekEntity = Entity TutorialWeek
