{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "  CREATE TABLE course (\n\
         \    id SERIAL PRIMARY KEY,\n\
         \    title TEXT NOT NULL\n\
         \  );\n\
         \\n\
         \  CREATE TABLE week (\n\
         \    id SERIAL PRIMARY KEY,\n\
         \    course_id INTEGER NOT NULL REFERENCES course(id),\n\
         \    number INTEGER NOT NULL\n\
         \  );\n\
         \\n\
         \  CREATE TABLE tutorial_week (\n\
         \    id SERIAL PRIMARY KEY,\n\
         \    tutorial_id INTEGER NOT NULL REFERENCES tutorial(id),\n\
         \    week_id INTEGER NOT NULL REFERENCES week(id)\n\
         \  );\n"

runDown = "  DROP TABLE tutorial_week;\n\
           \  DROP TABLE week;\n\
           \  DROP TABLE course;\n"
