{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Database.Migrate
import           Site

main = runMainSnap app $ do
  upSql runUp
  downSql runDown

runUp = "  ALTER TABLE step\n\
         \    ADD COLUMN video_code Text,\n\
         \    ADD COLUMN video_provider Text;\n"

runDown = "  ALTER TABLE step\n\
           \    DROP COLUMN video_code,\n\
           \    DROP COLUMN video_provider;\n"
