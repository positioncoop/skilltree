{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances,
    TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables,
    FlexibleContexts #-}

module Tutorial.Form where

import Control.Lens
import Prelude hiding ((++))
import Text.Digestive
import Snap.Plus
import Snap.Plus.Forms
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified System.IO as IO
import qualified Control.Monad.Trans.Resource as R
import Data.Conduit.ImageSize

import Tutorial.Types
import Application

untitledTutorial :: Int -> Int -> Tutorial
untitledTutorial a b = Tutorial a b "Untitled" Nothing

newForm :: Form Text AppHandler Tutorial
newForm = checkM "Tutorial overlaps" overlapping $
  untitledTutorial <$> "x" .: stringRead "Must be a number" Nothing
                   <*> "y" .: stringRead "Must be a number" Nothing
  where overlapping (Tutorial x y _ _) =
          do result <- runPersist (selectList [TutorialX ==. x, TutorialY <-. [y-1..y+1]]
                                              [LimitTo 1])
             return (null result)

moveForm :: TutorialEntity -> Form Text AppHandler Tutorial
moveForm (Entity key (Tutorial _ _ title iconPath)) = checkM "Tutorial overlaps" overlapping $
  Tutorial <$> "x" .: stringRead "Must be a number" Nothing
           <*> "y" .: stringRead "Must be a number" Nothing
           <*> pure title <*> pure iconPath
  where overlapping (Tutorial x y _ _) = do
          result <- runPersist (selectList [TutorialX ==. x, TutorialY <-. [y-1..y+1], TutorialId !=. key] [LimitTo 1])
          return (null result)

editForm :: Tutorial -> Form Text AppHandler Tutorial
editForm (Tutorial x y title mIconPath) = Tutorial x y <$> "title" .: nonEmpty (text (Just title))
                                          <*> "iconPath" .: moveFile mIconPath (enforceImageSize file)

moveFile :: Maybe FilePath -> Form Text AppHandler (Maybe FilePath) -> Form Text AppHandler (Maybe FilePath)
moveFile def = validateM mkMedia
  where  mkMedia :: Maybe FilePath -> AppHandler (Result Text (Maybe FilePath))
         mkMedia Nothing = return $ Success def
         mkMedia (Just _path) =
            do store <- use filestore
               url <- storeFile store _path
               return $ Success (Just $ unpack url)

enforceImageSize :: Form Text AppHandler (Maybe FilePath) -> Form Text AppHandler (Maybe FilePath)
enforceImageSize = checkM "Image must be a PNG, 60x60." $ \mFilePath -> case mFilePath of
  Nothing -> return True
  Just filePath ->
    liftIO $ R.runResourceT $
      do (_, inputH) <- R.allocate (IO.openFile filePath IO.ReadMode) IO.hClose
         info <- CB.sourceHandle inputH C.$$ sinkImageInfo
         case info of
           Just (size, PNG) -> return (width size == 60 && height size == 60)
           _ -> return False
