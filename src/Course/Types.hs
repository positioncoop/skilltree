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

module Course.Types where

import           Data.Aeson.Types
import           Database.Persist.TH
import           Database.Persist.Types
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Plus.Paths
import           Snap.Snaplet.Persistent (showKey)

import qualified Tutorial.Types          as T

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

instance Paths CourseId Course where
  indexPath _ = "/courses/"
