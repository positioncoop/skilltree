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
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Text.Digestive.Snap     (runForm)

import qualified Course.Types            as C
import           TutorialWeek.Types
import           Week.Queries
import           Week.Types

import           Application

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: C.CourseEntity -> [(Text, AppHandler ())]
routes centity = [ ("new", ifTop $ requireUser auth authCheck $ newH centity)
                 , ("delete", requireUser auth authCheck $ deleteH centity)
                 , (":week_id", ifTop showH)
                 , (":week_id/toggle_tutorial", requireUser auth authCheck $ toggleTutorialH centity)
                 ]

showH :: AppHandler ()
showH = do
  i <- requireParam "week_id"
  loggedIn <- with auth isLoggedIn
  ts <- if loggedIn then lookupTutorialsByWeek i
                    else lookupPublishedTutorialsByWeek i
  writeJSON ts

newH :: C.CourseEntity -> AppHandler ()
newH (Entity ckey _) = do
  nweeks <- runPersist $ count [WeekCourseId ==. ckey]
  runPersist $ insert (Week ckey (nweeks + 1))
  redirectReferer

deleteH :: C.CourseEntity -> AppHandler ()
deleteH (Entity ckey _) = do
  weeks <- runPersist $ selectList [WeekCourseId ==. ckey] [Desc WeekNumber, LimitTo 1]
  case weeks of
    (Entity k _:_) -> runPersist $ delete k
    _ -> return ()
  redirectReferer

toggleTutorialH :: C.CourseEntity -> AppHandler ()
toggleTutorialH (Entity ckey _) = do
  wkey <- requireParam "week_id"
  tkey <- requireParam "tutorial_id"
  let fil = [TutorialWeekTutorialId ==. tkey, TutorialWeekWeekId ==. wkey]
  c <- runPersist $ count fil
  runPersist $ if c == 0
                  then void $ insert (TutorialWeek tkey wkey)
                  else deleteWhere fil
  redirectReferer
