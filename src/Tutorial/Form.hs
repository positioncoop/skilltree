{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Tutorial.Form where

import Control.Lens
import Prelude hiding ((++))
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive
import Snap
import Snap.Snaplet.Persistent
import Database.Persist
import FileStore
import Forms

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified System.IO as IO
import qualified Control.Monad.Trans.Resource as R
import Control.Monad.Trans.Either (runEitherT, left, right)
import Data.Conduit.ImageSize

import Tutorial.Types
import Application

untitledTutorial :: Int -> Int -> Tutorial
untitledTutorial a b = Tutorial a b "Untitled" Nothing

newForm :: Form Text AppHandler Tutorial
newForm = checkM "Tutorial overlaps with existing tutorial" overlapping $
  untitledTutorial <$> "x" .: stringRead "Must be a number" Nothing
                   <*> "y" .: stringRead "Must be a number" Nothing
  where overlapping (Tutorial x y _ _) = do result <- runPersist (selectList [TutorialX ==. x, TutorialY ==. y] [LimitTo 1])
                                            return (result == [])

editForm :: Tutorial -> Form Text AppHandler Tutorial
editForm (Tutorial x y title mIconPath) = Tutorial x y <$> "title" .: nonEmpty (text (Just title))
                                          <*> "iconPath" .: moveFile mIconPath (enforceImageSize file)

moveFile :: Maybe FilePath -> Form Text AppHandler (Maybe FilePath) -> Form Text AppHandler (Maybe FilePath)
moveFile def existing = validateM mkMedia existing
  where  mkMedia :: Maybe FilePath -> AppHandler (Result Text (Maybe FilePath))
         mkMedia Nothing = return $ Success def
         mkMedia (Just _path) =
            do store <- use filestore
               url <- storeFile store _path Nothing
               return $ Success (Just $ T.unpack url)

enforceImageSize :: Form Text AppHandler (Maybe FilePath) -> Form Text AppHandler (Maybe FilePath)
enforceImageSize = checkM "Image must be a PNG, 60x60." $ \mFilePath -> case mFilePath of
  Nothing -> return True
  Just filePath -> do res <- liftIO $ R.runResourceT $ runEitherT $
                        do (_, inputH) <- lift $ R.allocate (IO.openFile filePath IO.ReadMode) IO.hClose
                           info <- CB.sourceHandle inputH C.$$ sinkImageInfo
                           case info of
                             Just (size, PNG) -> return (width size == 60 && height size == 60)
                             _ -> return False
                      return $ either (const False) id res
