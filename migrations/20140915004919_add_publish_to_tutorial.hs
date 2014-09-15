{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "ALTER TABLE tutorial ADD COLUMN publish text NOT NULL DEFAULT 'Draft';"

runDown = "ALTER TABLE tutorial DROP COLUMN IF EXISTS publish;"
