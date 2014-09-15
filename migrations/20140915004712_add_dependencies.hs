{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "  CREATE TABLE dependency (\n\
         \    id SERIAL PRIMARY KEY,\n\
         \    tutorial_id INTEGER NOT NULL REFERENCES tutorial(id),\n\
         \    dependency_id INTEGER NOT NULL REFERENCES tutorial(id)\n\
         \  );\n\
         \  CREATE INDEX dependency_tutorial_id_index ON dependency(tutorial_id);\n\
         \  CREATE INDEX dependency_dependency_id_index ON dependency(dependency_id);\n"

runDown = "DROP TABLE dependency;"
