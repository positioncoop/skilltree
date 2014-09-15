{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "  CREATE TABLE tutorial (\n\
         \    id serial PRIMARY KEY,\n\
         \    title varchar(255) NOT NULL,\n\
         \    x integer NOT NULL,\n\
         \    y integer NOT NULL\n\
         \  );\n"

runDown = "DROP TABLE tutorial;\n"
