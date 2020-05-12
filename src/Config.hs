module Config
 ( Config (..)
 , Shortcut (..)
 , appName
 , getConfig
 , findShortcut
 , saveToHistory )
where

import Data.Char     (isSpace)
import Data.List     (find, sort)

import System.Directory      as Dir
import System.FilePath.Posix as Path

import Utils (ifM)
import Completion (Dict, makeDict)


data Config = Cfg { history    :: [String]
                  , histAcc    :: Dict
                  , shortcuts  :: [Shortcut] }
   deriving Show

data Shortcut = SC { sc_name :: String, sc_pattern :: String }
   deriving Show

type Line = String


appName = "hrunner"

getAppUserDataDir = Dir.getAppUserDataDirectory appName


readOrCreateFile :: FilePath -> IO [Line]
readOrCreateFile f =
   ifM (Dir.doesFileExist f)
      (readFile f >>= return . filter (not . null) . remComments . lines)
      (writeFile f "" >> return [])


-- Remove lines starting with '#' (possibly following spaces)
remComments :: [String] -> [String]
remComments = filter (not . comment)
   where comment l = case dropWhile isSpace l of (c:r) -> c == '#' ; _ -> False


getConfig :: IO Config
getConfig = do
   cfgHome <- getAppUserDataDir
   Dir.createDirectoryIfMissing False cfgHome
   contsHist   <- readOrCreateFile (Path.combine cfgHome "history")
   contsShorts <- readOrCreateFile (Path.combine cfgHome "shortcuts")
   return Cfg { history   = mkHist contsHist
              , histAcc   = makeDict contsHist
              , shortcuts = mkShortcuts contsShorts }


mkHist :: [Line] -> [Line]
mkHist = id

mkShortcuts :: [Line] -> [Shortcut]
mkShortcuts = map mkSC
   where
      mkSC s = let (sc, cmd) = break isSpace (dropWhile isSpace s)
               in SC sc (dropWhile isSpace cmd)


findShortcut :: String -> [Shortcut] -> Maybe Shortcut
findShortcut x = find (\sc -> x == sc_name sc)


saveToHistory :: String -> IO ()
saveToHistory s = do
   cfgHome <- getAppUserDataDir
   let f = Path.combine cfgHome "history"
   appendFile f (s ++ "\n")
