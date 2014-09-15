{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "  CREATE TABLE step (\n\
         \    id serial PRIMARY KEY,\n\
         \    tutorial_id integer NOT NULL references tutorial(id),\n\
         \    content text NOT NULL,\n\
         \    ordinal integer NOT NULL\n\
         \  );\n"

runDown = "DROP TABLE step;"
