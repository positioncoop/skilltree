{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Course.Handlers where

import           Data.Aeson
import           Database.Persist
import           Prelude                 hiding ((++))
import           Snap.Extras.JSON
import           Snap.Plus
import           Snap.Plus.Handlers
import           Snap.Plus.Paths
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Text.Digestive.Snap     (runForm)

import           Course.Form
import           Course.Types
import qualified Week.Handlers
import qualified Week.Types              as W

import           Application

courseResource :: Resource Course
courseResource = Resource indexH (authorize newH) (const pass) (const pass) (authorize . deleteH)
                          [("weeks", route . Week.Handlers.nestedRoutes)]
                          []

routes :: [(Text, AppHandler ())]
routes = resourceRoutes courseResource

indexH :: AppHandler ()
indexH = routeFormats [([JSON], indexJsonH)]

indexJsonH :: AppHandler ()
indexJsonH = do
  courses <- runPersist $ selectList [] [] :: AppHandler [CourseEntity]
  coursesWithWeeks <- mapM (\c -> do wks <- lookupCourseWeeks c
                                     return (c, wks))
                           courses
  writeJSON $ map formatJSON coursesWithWeeks
  where formatJSON (Entity k (Course title), wks) =
          object [ "id" .= showKey k
                 , "title" .= title
                 , "weeks" .= wks]

newH :: AppHandler ()
newH = do
  response <- runForm "new" Course.Form.newForm
  case response of
    (_, Nothing) -> redirectReferer
    (_, Just course) -> do
      void $ runPersist $ insert course
      redirectReferer

deleteH :: Entity Course -> AppHandler ()
deleteH course@(Entity courseKey _) = do
  weeks <- lookupCourseWeeks course
  when (null weeks) $
    runPersist $ delete (courseKey :: Key Course)
  redirectReferer

lookupCourseWeeks :: CourseEntity -> AppHandler [W.WeekEntity]
lookupCourseWeeks (Entity key _) = runPersist $ selectList [W.WeekCourseId ==. key] []
