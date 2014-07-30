{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Splices where

import Prelude hiding ((++))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted
import Snap

import Database.Persist.Types
import qualified Snap.Snaplet.Persistent as P
import Database.Persist

import Tutorial.Types
import Step.Types
import qualified Step.Splices
import Helpers
import Application

entitySplice :: TutorialEntity -> Splices (Splice AppHandler)
entitySplice (Entity _id (Tutorial _x _y _title _iconPath)) = do
  "tutorialId" ## textSplice $ P.showKey _id
  "tutorialX" ## textSplice $ tshow _x
  "tutorialY" ## textSplice $ tshow _y
  "tutorialTitle" ## textSplice _title
  "tutorialIconPath" ## textSplice $ maybe "" T.pack _iconPath
  "tutorialSteps" ## do steps <- lift $ P.runPersist $ selectList [StepTutorialId ==. P.mkInt _id] [Asc StepOrdinal]
                        mapSplices (runChildrenWith . Step.Splices.entitySplice) steps
