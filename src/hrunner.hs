{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events      as Ev
import System.Directory (doesFileExist, executable, getPermissions)

import qualified System.FilePath.Posix as Path
import qualified System.Process        as Sys

import PrefixTree
import Subst
import Utils

import Config (Config, appName)
import qualified Config


data Command = Pass | Run [String] | Calc String


helpText = "Enter command to run.\n:\tfor shortcut.\n=\tfor expression evaluation."


main = do
  config <- Config.getConfig

  initGUI
  window <- windowNew
  vbox   <- vBoxNew True 5
  text   <- labelNew (Just helpText)
  entry  <- entryNew
  btnBox <- hButtonBoxNew
  btnCancel <- buttonNew
  btnGo     <- buttonNew

  set window [ windowTitle          := appName
             , containerBorderWidth := 10
             , containerChild       := vbox
             , windowWindowPosition := WinPosCenter
	     , windowDefaultWidth   := 400 ]

  set btnCancel [ buttonLabel := "Cancel" ]
  set btnGo     [ buttonLabel := "Run", widgetCanDefault := True ]

  containerAdd vbox text
  containerAdd vbox entry
  containerAdd vbox btnBox
  containerAdd btnBox btnCancel
  containerAdd btnBox btnGo

  -- Set font size of text entry
  do
    d <- fontDescriptionNew
    fontDescriptionSetSize d 14
    widgetModifyFont entry (Just d)

  widgetGrabDefault btnGo

  onClicked btnCancel mainQuit
  onClicked btnGo (goAction config entry)
  onKeyPress window (evHandler config entry)
  onDestroy window mainQuit
  
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


evHandler :: Config -> Entry -> Event -> IO Bool
evHandler _ _ (Key { Ev.eventKeyName = "Escape" }) =
  mainQuit >> return True

-- This should not be needed, but the default action does not seem to work.
evHandler cfg e (Key { Ev.eventKeyName = "Return" }) = 
  goAction cfg e >> return True

evHandler _ e (Key { Ev.eventModifier = mods, Ev.eventKeyChar = Just 'u' })
  | Control `elem` mods = entrySetText e "" >> return True

evHandler _ e (Key { Ev.eventModifier = mods, Ev.eventKeyChar = Just 'w' })
  | Control `elem` mods = do
      t <- entryGetText e
      entrySetText e (unwords $ init $ words t)
      editableSetPosition e (negate 1)
      return True

evHandler cfg e (Key { Ev.eventKeyChar = Just c }) = do
  (i,j) <- editableGetSelectionBounds e
  editableDeleteText e i j
  p <- editableInsertText e [c] =<< editableGetPosition e
  t <- entryGetText e
  case suggest (Config.histAcc cfg) t of
   Nothing  -> do
     editableSetPosition e p
     return True
   Just suf -> do
     p' <- editableInsertText e suf p
     editableSelectRegion e p' p
     return True

evHandler _ _ _ = return False


mkCommand :: Config -> String -> Command
mkCommand cfg = mk . words
   where
      mk :: [String] -> Command
      mk ("=":es) = Calc (unwords es)
      mk ((':':s):ps) = maybe Pass
                              (\sc -> Run $ words $ subst (Config.sc_pattern sc) ps)
                              (Config.findShortcut s (Config.shortcuts cfg))
      mk cmd          = Run cmd



entrySelectAll :: Entry -> IO ()
entrySelectAll e = do
  l <- length `fmap` entryGetText e
  editableSelectRegion e 0 l


-- tryRunCommand :: String -> IO Bool
-- tryRunCommand cmdStr = do
--   let (cmd,argl) = break isSpace cmdStr
--   ps <- Path.getSearchPath
--   p  <- tryPaths (allPaths cmd ps)
--   case p of
--     Nothing -> return False
--     Just c  -> Sys.runCommand (c ++ argl) >> return True


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
    else map (`Path.combine` f) ps

-- Return the first executable FilePath or Nothing
-- if no given file path is executable.
tryPaths :: [FilePath] -> IO (Maybe FilePath)
tryPaths []     = return Nothing
tryPaths (p:ps) = do
  putStrLn ("Trying " ++ p)
  ifM (doesFileExist p `andM` (getPermissions p >>= return . executable))
      (return $ Just p)
      (tryPaths ps)



-- NOT USED
-- editableSelectRegion has no effect when called from an editableChanged
-- signal handler.
changeHandler :: IORef (ConnectId Entry) -> Dict -> Entry -> String -> Int -> IO Int
changeHandler ref d e s p = do
   putStrLn (s ++ " " ++ show p) >> return p
   t <- entryGetText e
   case suggest d t of
      Nothing  -> return p
      Just suf -> do
	 id <- readIORef ref
	 signalBlock id
	 p' <- editableInsertText e suf p
	 putStrLn (t ++ " + " ++ suf ++ "\t" ++ show p ++ "->" ++ show p')
	 signalUnblock id
	 stopInsertText id
	 editableSelectRegion e p p'
	 return p

