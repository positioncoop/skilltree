{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "ALTER TABLE tutorial ADD COLUMN icon_path varchar(255);"

runDown = "ALTER TABLE tutorial DROP COLUMN IF EXISTS icon_path;"
