module Dependency.Queries where

import Snap.Plus
import qualified Snap.Snaplet.Persistent as P
import Database.Esqueleto
import Tutorial.Types
import Dependency.Types

import Application

reorder :: (TutorialEntity, TutorialEntity) -> (TutorialEntity, TutorialEntity)
reorder (target, source) = if tutorialX (entityVal target) <= tutorialX (entityVal source) then (target, source) else (source, target)

lookupAllDependencyPairs :: AppHandler [(TutorialEntity, TutorialEntity)]
lookupAllDependencyPairs = map reorder <$>
  (P.runPersist $ select $ from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    return (target, source))

lookupPublishedDependencyPairs  :: AppHandler [(TutorialEntity, TutorialEntity)]
lookupPublishedDependencyPairs = map reorder <$>
  (P.runPersist $ select $ from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    where_ (val Published ==. source ^. TutorialPublish)
    where_ (val Published ==. target ^. TutorialPublish)
    return (target, source))
