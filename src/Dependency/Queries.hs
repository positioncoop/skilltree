module Dependency.Queries where

import           Database.Esqueleto
import           Dependency.Types
import           Snap.Plus
import qualified Snap.Snaplet.Persistent as P
import           Tutorial.Types

import           Application

reorder :: (TutorialEntity, DependencyEntity, TutorialEntity) -> (TutorialEntity, DependencyEntity, TutorialEntity)
reorder (target, dep, source) =
  if tutorialX (entityVal target) <= tutorialX (entityVal source)
  then (target, dep, source)
  else (source, dep, target)

lookupAllDependencyPairs :: AppHandler [(TutorialEntity, DependencyEntity, TutorialEntity)]
lookupAllDependencyPairs = map reorder <$>
  (P.runPersist $ select $ from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    return (target, djoin, source))

lookupPublishedDependencyPairs  :: AppHandler [(TutorialEntity, DependencyEntity, TutorialEntity)]
lookupPublishedDependencyPairs = map reorder <$>
  (P.runPersist $ select $ from $ \(target `InnerJoin` djoin `InnerJoin` source) -> do
    on (source ^. TutorialId ==. djoin ^. DependencyTutorialId)
    on (target ^. TutorialId ==. djoin ^. DependencyDependencyId)
    where_ (val Published ==. source ^. TutorialPublish)
    where_ (val Published ==. target ^. TutorialPublish)
    return (target, djoin, source))
