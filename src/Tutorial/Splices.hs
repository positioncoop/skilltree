{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Splices where

import Prelude hiding ((++))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted

import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P

import Tutorial.Types
import Helpers
import Application

entitySplice :: TutorialEntity -> Splices (Splice AppHandler)
entitySplice (Entity _id (Tutorial _x _y _title _iconPath)) = do
  "tutorialId" ## textSplice $ P.showKey _id
  "tutorialX" ## textSplice $ tshow _x
  "tutorialY" ## textSplice $ tshow _y
  "tutorialTitle" ## textSplice _title
  "tutorialIconPath" ## textSplice $ maybe "" T.pack _iconPath
