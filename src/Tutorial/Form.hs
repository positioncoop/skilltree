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
editForm (Tutorial x y title mIconPath) = validateM mkMedia $ Tutorial x y <$> "title" .: text (Just title) <*> "iconPath" .: file
    where mkMedia :: Tutorial -> AppHandler (Result Text Tutorial)
          mkMedia (Tutorial a b c Nothing) = return $ Success $ Tutorial a b c mIconPath
          mkMedia (Tutorial a b c (Just _path)) =
            do store <- use filestore
               res <- liftIO $ R.runResourceT$ runEitherT $
                 do (_, inputH) <- lift $ R.allocate (IO.openFile _path IO.ReadMode) IO.hClose
                    info <- CB.sourceHandle inputH C.$$ sinkImageInfo
                    case info of
                      Just (size, PNG) -> return (width size == 60 && height size == 60)
                      _ -> return False
               case res of
                 Right True -> do url <- storeFile store _path Nothing
                                  return $ Success $ Tutorial a b c (Just $ T.unpack url)
                 _ -> return $ Error "Image must be a PNG, 60x60 pixels"
