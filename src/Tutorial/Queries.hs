module Tutorial.Queries where

import qualified Snap.Snaplet.Persistent as P
import Database.Persist
import Tutorial.Types
import Step.Types

import Application

lookupTutorialSteps :: TutorialEntity -> AppHandler [StepEntity]
lookupTutorialSteps (Entity key _) = P.runPersist $ selectList [StepTutorialId ==. P.mkInt key] [Asc StepOrdinal]
