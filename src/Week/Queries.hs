module Week.Queries where

import qualified Snap.Snaplet.Persistent as P
import Database.Esqueleto
import Tutorial.Types
import Tutorial.Publish
import Week.Types
import TutorialWeek.Types

import Application

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
