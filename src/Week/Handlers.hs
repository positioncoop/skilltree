{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Week.Handlers where

import           Data.Aeson
import           Database.Persist
import           Prelude                 hiding ((++))
import           Snap.Extras.CoreUtils
import           Snap.Extras.JSON
import           Snap.Plus
import           Snap.Plus.Handlers
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Text.Digestive.Snap     (runForm)

import           Course.Types
import           TutorialWeek.Types
import           Week.Queries
import           Week.Types

import           Application

nestedWeekResource :: CourseEntity -> Resource Week
nestedWeekResource c = Resource pass (authorize $ newH c) (const pass) (const pass) (const pass)
                                [] [("delete", authorize $ deleteWeekH c)]

weekResource :: Resource Week
weekResource = Resource pass pass showH (const pass) (const pass) [("toggle_tutorial", toggleTutorialH)] []

routes = resourceRoutes weekResource
nestedRoutes c = resourceRoutes (nestedWeekResource c)


showH :: WeekEntity -> AppHandler ()
showH (Entity weekKey _) = do
  loggedIn <- with auth isLoggedIn
  ts <- if loggedIn then lookupTutorialsByWeek weekKey
                    else lookupPublishedTutorialsByWeek weekKey
  writeJSON ts

newH :: CourseEntity -> AppHandler ()
newH (Entity ckey _) = do
  nweeks <- runPersist $ count [WeekCourseId ==. ckey]
  runPersist $ insert (Week ckey (nweeks + 1))
  redirectReferer

deleteWeekH :: CourseEntity -> AppHandler ()
deleteWeekH (Entity ckey _) = do
  weeks <- runPersist $ selectList [WeekCourseId ==. ckey] [Desc WeekNumber, LimitTo 1]
  case weeks of
    (Entity k _:_) ->
      do tutorialWeeks <- runPersist $ count [TutorialWeekWeekId ==. k]
         when (tutorialWeeks == 0) $ runPersist $ delete k
    _ -> return ()
  redirectReferer

toggleTutorialH :: WeekEntity -> AppHandler ()
toggleTutorialH (Entity wkey _) = do
  tkey <- requireParam "tutorial_id"
  let fil = [TutorialWeekTutorialId ==. tkey, TutorialWeekWeekId ==. wkey]
  c <- runPersist $ count fil
  runPersist $ if c == 0
                  then void $ insert (TutorialWeek tkey wkey)
                  else deleteWhere fil
  redirectReferer
