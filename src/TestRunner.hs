{-# LANGUAGE OverloadedStrings #-}

module TestRunner where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar,
                                          tryPutMVar)
import           Control.Monad           (unless, void)
import qualified Data.Text               as T
import           System.Exit             (ExitCode (..))
import           System.FSNotify         (withManager)
import           System.FSNotify.Devel   (treeExtExists)
import           System.IO               (hGetContents)
import           System.Process          (CreateProcess (..),
                                          StdStream (CreatePipe), createProcess,
                                          shell, system, waitForProcess)

main :: IO ()
main = do shouldRun <- newEmptyMVar :: IO (MVar ())
          run
          withManager (\man ->
                         do mapM_ (\d -> treeExtExists man d "hs" (triggerRun shouldRun)) ["."]
                            forkIO (testRunner shouldRun)
                            loopForever)
  where loopForever = do threadDelay 1000
                         loopForever
        triggerRun mv f = unless (any (`T.isInfixOf` T.pack (show f)) ["flycheck", "#"])
                                 (void $ tryPutMVar mv ())
        testRunner mv = do _ <- takeMVar mv
                           run
                           testRunner mv
        run = do (_,Just hout, Just herr, handle) <- createProcess $ (shell "runghc -no-user-package-db -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d -isrc src/Test.hs"){ std_out = CreatePipe, std_err = CreatePipe }
                 res <- waitForProcess handle
                 case res of
                   ExitSuccess -> do err <- hGetContents hout
                                     out <- hGetContents herr
                                     putStr err
                                     putStr out
                   ExitFailure _ -> do err <- hGetContents hout
                                       out <- hGetContents herr
                                       void $ system $ "notify-send -u normal -t 2000 'Tests Failed To Run' '" ++ stripQuotes err ++ stripQuotes out ++ "'"
          where stripQuotes = filter (/= '\'')
