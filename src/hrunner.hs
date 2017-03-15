{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import Graphics.UI.Gtk
import System.Directory (doesFileExist, executable, getPermissions)
import System.FilePath  ((</>))

import qualified Data.Text        as Text
import qualified System.FilePath  as Path
import qualified System.Process   as Sys

import Completion
import Subst
import Utils

import Config (Config, appName)
import qualified Config


data Command = Pass | Run [String] | Calc String


helpText = text $ concat
   [ "Enter command to run.\n"
   , ":\tfor shortcut.\n"
   , "=\tfor expression evaluation."
   ]


text :: String -> Text
text = Text.pack


main = do
  config <- Config.getConfig

  initGUI

  window    <- windowNew
  vbox      <- vBoxNew True 5
  label     <- labelNew (Just helpText)
  entry     <- entryNew
  btnBox    <- hButtonBoxNew
  btnCancel <- buttonNew
  btnGo     <- buttonNew

  set window [ windowTitle          := appName
             , containerBorderWidth := 10
             , containerChild       := vbox
             , windowWindowPosition := WinPosCenter
             , windowDefaultWidth   := 400 ]

  set btnCancel [ buttonLabel := text "Cancel" ]
  set btnGo     [ buttonLabel := text "Run", widgetCanDefault := True ]

  containerAdd vbox label
  containerAdd vbox entry
  containerAdd vbox btnBox
  containerAdd btnBox btnCancel
  containerAdd btnBox btnGo

  -- Set font size of text entry
  do
    d <- fontDescriptionNew
    fontDescriptionSetSize d 14
    widgetModifyFont entry (Just d)

  widgetSetCanDefault btnGo True
  widgetGrabDefault btnGo
  entrySetActivatesDefault entry True

  -- Reference to list of completion suffixes
  complRef <- newIORef []

  on btnCancel buttonActivated $ liftIO mainQuit
  on btnGo buttonActivated $ liftIO $ goAction config entry
  on window keyPressEvent $ tryEvent $ keyPressHandler config entry complRef

  on window deleteEvent (liftIO mainQuit >> return True)

  widgetShowAll window
  mainGUI


goAction :: Config -> Entry -> IO ()
goAction cfg e = do
   input <- entryGetText e
   case mkCommand cfg input of
      Pass    -> entrySelectAll e
      Run cmd ->
        ifM (tryRunCommand cmd)
          (Config.saveToHistory (unwords $ words input) >> mainQuit)
          (entrySelectAll e)
      Calc expr -> do
         -- Use python to calculate expressions
         ret <- Sys.readProcess "python" ["-c", "from math import *\nprint (" ++ expr ++ ")"] []
         entrySetText e (init ret)
         entrySelectAll e


keyPressHandler :: Config -> Entry -> IORef [String] -> EventM EKey ()
keyPressHandler cfg entry complRef =
  msum
  [ do "Escape" <- eventKeyName
       liftIO mainQuit

  , do "u" <- eventKeyName
       [Control] <- eventModifier
       liftIO $ writeIORef complRef []
       liftIO $ entrySetText entry (text "")

  , do "w" <- eventKeyName
       [Control] <- eventModifier
       liftIO $ writeIORef complRef []
       liftIO $ do
         t <- entryGetText entry
         entrySetText entry (unwords $ init $ words t)
         editableSetPosition entry (negate 1)

  , do Just '\t' <- fmap keyToChar eventKeyVal
       liftIO $ do
          modifyIORef complRef rotateL
          suffixes <- readIORef complRef
          suggestCompletion entry suffixes

  , do Just c <- fmap keyToChar eventKeyVal
       -- Ensure we don't consume backspace and return keys:
       guard (c /= '\b' && c /= '\r')

       liftIO $ do
         (i,j) <- editableGetSelectionBounds entry
         editableDeleteText entry i j
         p <- editableInsertText entry [c] =<< editableGetPosition entry
         t <- entryGetText entry
         let suffixes = getSuffixes (Config.histAcc cfg) t
         writeIORef complRef suffixes
         case suffixes of
           []  -> do
             editableSetPosition entry p
           suffixes@(suf:_) -> do
             p' <- editableInsertText entry suf p
             editableSelectRegion entry p' p
  ] -- end keyPressHandler


suggestCompletion :: Entry -> [String] -> IO ()
suggestCompletion entry suffixes = do
   (i,j) <- editableGetSelectionBounds entry
   editableDeleteText entry i j
   case suffixes of
      (suffix:_) -> do
         p <- editableGetPosition entry
         p' <- editableInsertText entry suffix p
         editableSelectRegion entry p' p
      _ ->
         return ()


mkCommand :: Config -> String -> Command
mkCommand cfg = mk . words
   where
      mk :: [String] -> Command
      mk ("=":es)     = Calc (unwords es)
      mk ((':':s):ps) = maybe Pass
                              (\sc -> Run $ words $ subst (Config.sc_pattern sc) ps)
                              (Config.findShortcut s (Config.shortcuts cfg))
      mk cmd          = Run cmd


entrySelectAll :: Entry -> IO ()
entrySelectAll e = do
  l <- Text.length <$> entryGetText e
  editableSelectRegion e 0 l


tryRunCommand :: [String] -> IO Bool
tryRunCommand (cmd:argl) = do
  ps <- Path.getSearchPath
  p  <- tryPaths (allPaths cmd ps)
  case p of
    Nothing -> return False
    Just c  -> do
      putStrLn ("DEBUG: " ++ show (c ++ " " ++ unwords argl))
      Sys.createProcess (Sys.proc c argl)
      return True


allPaths :: FilePath -> [FilePath] -> [FilePath]
allPaths f ps =
  if Path.isAbsolute f
    then [f]
    else [p </> f | p <- ps]


-- Return the first executable FilePath or Nothing
-- if no given file path is executable.
tryPaths :: [FilePath] -> IO (Maybe FilePath)
tryPaths []     = return Nothing
tryPaths (p:ps) = do
  putStrLn ("Trying " ++ p)
  ifM (doesFileExist p `andM` (executable <$> getPermissions p))
      (return $ Just p)
      (tryPaths ps)
