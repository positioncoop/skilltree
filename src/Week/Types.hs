{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Week.Types where

import           Data.Aeson.Types
import           Database.Persist.TH
import           Database.Persist.Types
import           Prelude                 hiding ((++))
import           Snap.Plus
import           Snap.Snaplet.Persistent (showKey)

import qualified Course.Types            as C

share [mkPersist sqlSettings] [persistLowerCase|
Week
  courseId C.CourseId
  number Int
  deriving Show
  deriving Eq
|]

type WeekEntity = Entity Week

instance ToJSON (Entity Week) where
  toJSON (Entity key (Week courseId number)) =
    object ["id" .= showKey key, "number" .= number]

weekPath :: WeekEntity -> Text
weekPath (Entity key _) = "/weeks/" ++ showKey key

weekDeletePath :: WeekEntity -> Text
weekDeletePath entity = weekPath entity ++ "/delete"
