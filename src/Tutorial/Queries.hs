module Tutorial.Queries where

import qualified Snap.Snaplet.Persistent as P
import Database.Esqueleto
import Tutorial.Types
import Dependency.Types
import Step.Types

import Application

lookupAllTutorials :: AppHandler [TutorialEntity]
lookupAllTutorials = P.runPersist $ select $ from return

lookupPublishedTutorials :: AppHandler [TutorialEntity]
lookupPublishedTutorials =
  P.runPersist $ select $ from $ \tutorial -> do
    where_ (tutorial ^. TutorialPublish ==. val Published)
    return tutorial

lookupTutorialSteps :: TutorialEntity -> AppHandler [StepEntity]
lookupTutorialSteps (Entity key _) =
  P.runPersist $ select $ from $ \step -> do
    where_ (step ^. StepTutorialId ==. val (P.mkInt key))
    orderBy [asc (step ^. StepOrdinal)]
    return step

lookupTutorialDependencies :: TutorialEntity -> AppHandler [(TutorialEntity, DependencyEntity)]
lookupTutorialDependencies (Entity tutorialKey _) =
  P.runPersist $ select $ from $ \(target `InnerJoin` djoin) -> do
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    where_ (val tutorialKey ==. djoin ^. DependencyTutorialId)
    return (target, djoin)
