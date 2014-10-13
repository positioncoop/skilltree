module Week.Queries where

import           Database.Esqueleto
import qualified Snap.Snaplet.Persistent as P
import           Tutorial.Types
import           TutorialWeek.Types
import           Week.Types

import           Application

lookupTutorialsByWeek :: Key Week -> AppHandler [TutorialEntity]
lookupTutorialsByWeek k =
  P.runPersist $ select $ from $
      \(tutorial `InnerJoin` tw) ->
          do on (tutorial ^. TutorialId ==. tw ^. TutorialWeekTutorialId)
             where_ (val k ==. tw ^. TutorialWeekWeekId)
             return tutorial


lookupPublishedTutorialsByWeek :: Key Week -> AppHandler [TutorialEntity]
lookupPublishedTutorialsByWeek k =
  P.runPersist $ select $ from $
      \(tutorial `InnerJoin` tw) ->
          do on (tutorial ^. TutorialId ==. tw ^. TutorialWeekTutorialId)
             where_ (val Published ==. tutorial ^. TutorialPublish)
             where_ (val k ==. tw ^. TutorialWeekWeekId)
             return tutorial
