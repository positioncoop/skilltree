module Tutorial.Queries where

import qualified Snap.Snaplet.Persistent as P
--import Database.Persist (selectList, delete, SelectOpt(Asc))
import Database.Esqueleto
import Tutorial.Types
import Dependency.Types
import Step.Types

import Application

lookupTutorialSteps :: TutorialEntity -> AppHandler [StepEntity]
lookupTutorialSteps (Entity key _) = P.runPersist $ select $
                                     from $ \step -> do
                                       where_ (step ^. StepTutorialId ==. val (P.mkInt key))
                                       orderBy [asc (step ^. StepOrdinal)]
                                       return step

lookupTutorialDependencies :: TutorialEntity -> AppHandler [(DependencyEntity, TutorialEntity)]
lookupTutorialDependencies (Entity tutorialKey _) =
  P.runPersist $ select $
  from $ \(target `InnerJoin` djoin) -> do
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    where_ (val tutorialKey ==. djoin ^. DependencyTutorialId)
    return (djoin, target)

lookupAllDependencyPairs :: AppHandler [(TutorialEntity, TutorialEntity)]
lookupAllDependencyPairs =
  P.runPersist $ select $
  from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    return (target, source)
