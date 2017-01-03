{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import Graphics.UI.Gtk
import System.Directory (doesFileExist, executable, getPermissions)

import qualified Data.Text             as Text
import qualified System.FilePath.Posix as Path
import qualified System.Process        as Sys

import PrefixTree
import Subst
import Utils

import Config (Config, appName)
import qualified Config


data Command = Pass | Run [String] | Calc String


helpText = text "Enter command to run.\n:\tfor shortcut.\n=\tfor expression evaluation."


-- For older gtk, using String instead of Text, it should be possible
-- to make this code work by changing this to the identity function.
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

  widgetGrabDefault btnGo

  on btnCancel buttonActivated $ liftIO mainQuit
  on btnGo buttonActivated $ liftIO $ goAction config entry
  on window keyPressEvent $ tryEvent $ keyPressHandler config entry

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


keyPressHandler :: Config -> Entry -> EventM EKey ()
keyPressHandler cfg entry =
  msum
  [ do "Escape" <- eventKeyName
       liftIO mainQuit

    -- This should be taken care of by the default button, but that
    -- does not work. I do not know why.
  , do "Return" <- eventKeyName
       liftIO $ goAction cfg entry

  , do "u" <- eventKeyName
       [Control] <- eventModifier
       liftIO $ entrySetText entry (text "")

  , do "w" <- eventKeyName
       [Control] <- eventModifier
       liftIO $ do
         t <- entryGetText entry
         entrySetText entry (unwords $ init $ words t)
         editableSetPosition entry (negate 1)

  , do Just c <- fmap keyToChar eventKeyVal
       liftIO $ do
         (i,j) <- editableGetSelectionBounds entry
         editableDeleteText entry i j
         p <- editableInsertText entry [c] =<< editableGetPosition entry
         t <- entryGetText entry
         case suggest (Config.histAcc cfg) t of
           Nothing  -> do
             editableSetPosition entry p
           Just suf -> do
             p' <- editableInsertText entry suf p
             editableSelectRegion entry p' p
  ] -- end keyPressHandler


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
  l <- Text.length `fmap` entryGetText e
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
