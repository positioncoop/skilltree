{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Tutorial.Types where

import Prelude hiding ((++))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.Types
import Database.Persist.TH
import Data.Aeson.Types
import Snap.Snaplet.Persistent (showKey, showKeyBS)

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

tutorialPath :: TutorialEntity -> ByteString
tutorialPath (Entity key _) = "/tutorials/" ++ (showKeyBS key) ++ "/edit"