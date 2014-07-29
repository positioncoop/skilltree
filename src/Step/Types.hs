{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}

module Step.Types where

import Data.Text (Text)
import Database.Persist.Types
import Database.Persist.TH

import Application ()

share [mkPersist sqlSettings] [persistLowerCase|
Step
  tutorialId Int
  content Text
  ordinal Int
  deriving Show
  deriving Eq
|]

type StepEntity = Entity Step
