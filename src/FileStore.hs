module FileStore where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Text           (Text)
import qualified Data.Text           as T
import           System.Directory    (copyFile, doesFileExist, removeFile)
import           System.FilePath     (addExtension, pathSeparator,
                                      takeExtension)
import           System.Random       (randomRIO)

data FileStore = Directory FilePath

storeFile :: (Functor m, MonadIO m) => FileStore -> FilePath -> m Text
storeFile store@(Directory dir) old =
  do id' <- show <$> liftIO (randomRIO (10000,9999999) :: IO Double)
     let ext = takeExtension old
     let new = addExtension id' ext
     let full = dir ++ [pathSeparator] ++ new
     e <- liftIO $ doesFileExist full
     if e
       then storeFile store old
       else (do liftIO $ copyFile old full
                return $ T.pack $ "/store/" ++ new)

deleteFile :: (Functor m, MonadIO m) => FileStore -> Text -> m ()
deleteFile (Directory dir) url =
  do let file = T.unpack $ T.drop 7 url
     let path = dir ++ [pathSeparator] ++ file
     e <- liftIO $ doesFileExist path
     when e $ liftIO $ removeFile path
