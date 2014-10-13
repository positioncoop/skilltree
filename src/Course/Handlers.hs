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

authCheck :: AppHandler ()
authCheck = redirect "/auth/login"

routes :: [(Text, AppHandler ())]
routes = [ ("", ifTop indexH)
         , ("new", ifTop $ requireUser auth authCheck newH)
         , (":id/delete", requireUser auth authCheck deleteH)
         , (":id/weeks", do i <- requireParam "id"
                            course <- require $ runPersist $ get i
                            route $ Week.Handlers.routes $ Entity i course)
         ]

indexH :: AppHandler ()
indexH = do
  loggedIn <- with auth isLoggedIn
  courses <- runPersist $ selectList [] [] :: AppHandler [CourseEntity]
  coursesWithWeeks <-
   mapM (\c@(Entity key _) -> do wks <- runPersist $ selectList [W.WeekCourseId ==. key] []
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
    (_, Nothing) -> redirect "/"
    (_, Just course) -> do
      void $ runPersist $ insert course
      redirect "/"

deleteH :: AppHandler ()
deleteH = do
  courseKey <- requireParam "id"
  runPersist $ delete (courseKey :: Key Course)
  redirectReferer
