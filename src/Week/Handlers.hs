{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Week.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Auth
import Snap.Snaplet.Persistent
import Snap.Extras.CoreUtils
import Snap.Extras.JSON
import Data.Aeson
import Database.Persist
import Text.Digestive.Snap (runForm)

import Week.Types
import TutorialWeek.Types
import qualified Course.Types as C

import Application

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: C.CourseEntity -> [(Text, AppHandler ())]
routes centity = [ ("new", ifTop $ requireUser auth authCheck $ newH centity)
                 , ("delete", requireUser auth authCheck $ deleteH centity)
                 , (":id/toggle_tutorial", requireUser auth authCheck $ addTutorialH centity)
                 ]

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

addTutorialH :: C.CourseEntity -> AppHandler ()
addTutorialH (Entity ckey _) = do
  wkey <- getParam "id"
  tkey <- getParam "tutorial_id"
  let fil = [TutorialWeekTutorialId ==. tkey, TutorialWeekWeekId ==. wkey]
  c <- runPersist $ count fil
  runPersist $ if c == 0
                  then void $ insert (TutorialWeek tkey wkey)
                  else deleteWhere fil
  redirectReferer
