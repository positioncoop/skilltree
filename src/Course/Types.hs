{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Course.Types where

import Prelude hiding ((++))
import Snap.Plus
import Database.Persist.Types
import Database.Persist.TH
import Snap.Snaplet.Persistent (showKey)
import Data.Aeson.Types

import qualified Tutorial.Types as T

share [mkPersist sqlSettings] [persistLowerCase|
Course
  title Text
  deriving Show
  deriving Eq
|]

type CourseEntity = Entity Course

coursePath :: CourseEntity -> Text
coursePath (Entity key _) = "/courses/" ++ showKey key

courseDeletePath :: CourseEntity -> Text
courseDeletePath entity = coursePath entity ++ "/delete"
