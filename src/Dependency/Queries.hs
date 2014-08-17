module Dependency.Queries where

import qualified Snap.Snaplet.Persistent as P
import Database.Esqueleto
import Tutorial.Types
import Dependency.Types

import Application

lookupAllDependencyPairs :: AppHandler [(TutorialEntity, TutorialEntity)]
lookupAllDependencyPairs =
  P.runPersist $ select $ from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    return (target, source)

lookupPublishedDependencyPairs  :: AppHandler [(TutorialEntity, TutorialEntity)]
lookupPublishedDependencyPairs =
  P.runPersist $ select $ from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    where_ (val Published ==. source ^. TutorialPublish)
    where_ (val Published ==. target ^. TutorialPublish)
    return (target, source)
