{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Process
    ( createProcess,
      proc,
      readProcessWithExitCode,
      waitForProcess,
      CreateProcess(std_err, std_in, std_out),
      StdStream(UseHandle, CreatePipe) )
import System.IO.Temp ( emptySystemTempFile )
import Data.List (foldl', isPrefixOf)
import System.Environment ( getArgs, getEnv, setEnv )
import System.Exit ( ExitCode(..), exitWith )
import System.IO
    ( stderr, hClose, hPutStrLn, openFile, IOMode(AppendMode) )
import System.Directory ( findExecutable )
import Data.Aeson ( eitherDecode, FromJSON )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import Debug.Trace ( traceShowId )

data Settings = Settings
  { repository :: String
  , password :: String
  , excludes :: [String]
  , files :: [String]
  , maxFileSize :: Maybe String
  , hourlies :: Int
  , dailies :: Int
  , weeklies :: Int
  , monthlies :: Int
  , yearlies :: Int
  } deriving (Generic, Show)

instance FromJSON Settings

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      json <- BL.readFile x
      case eitherDecode json of
             Left e -> do
               hPutStrLn stderr e
               exitWith $ ExitFailure 3
             Right settings -> backup settings
    _ -> do
      hPutStrLn stderr "Usage: backup-to-storagebox path-to-settings.json"
      exitWith $ ExitFailure 1

runRestic :: Settings -> FilePath -> FilePath -> [String] -> IO ExitCode
runRestic settings resticPath logFile extraArgs = do
  logH <- openFile logFile AppendMode
  (Just pipeIn, _, _, ph) <- createProcess
    (proc "caffeinate"
     ([ "-i"
      , resticPath
      , "--repo"
      , repository settings
      ] ++ extraArgs)
     ){ std_in = CreatePipe
      , std_out = UseHandle logH
      , std_err = UseHandle logH }
  hPutStrLn pipeIn (password settings)
  hClose pipeIn
  waitForProcess ph

backup :: Settings -> IO ()
backup settings = do
  tempfile <- emptySystemTempFile "backup-to-storagebox"
  hPutStrLn stderr $ "Logging at " ++ tempfile
  restic <- getResticPath
  hPutStrLn stderr $ "Using restic at " ++ restic

  -- unlock just in case there are stale locks
  _ <- runRestic settings restic tempfile [ "unlock", "--verbose" ]

  bec <- runRestic settings restic tempfile $
      [ "backup"
      , "--exclude-caches"
      ]
      ++ maybe [] (\x -> [ "--exclude-larger-than" , x ]) (maxFileSize settings)
      ++ concatMap (\e -> ["--exclude", e]) (excludes settings)
      ++ files settings

  -- use restic diff to see which files changed
  bout <- readFile tempfile
  dec <- case traceShowId $! extractSnapshots bout of
    (Just parent, Just snapshot) ->
      runRestic settings restic tempfile
        [ "diff"
        , parent
        , snapshot
        , "--verbose"
        ]
    _ -> do
      hPutStrLn stderr "Could not extract snapshots for diff."
      pure $ ExitFailure 7

  fec <- case bec of
           ExitFailure _ -> pure bec
           ExitSuccess -> do
             hPutStrLn stderr "Pruning repository..."
             runRestic settings restic tempfile
                [ "forget"
                , "--quiet"
                , "--prune"
                , "--keep-hourly"
                , show (hourlies settings)
                , "--keep-daily"
                , show (dailies settings)
                , "--keep-weekly"
                , show (weeklies settings)
                , "--keep-monthly"
                , show (monthlies settings)
                , "--keep-yearly"
                , show (yearlies settings)
                ]

  let message = "Backup finished" <>
       (if bec == ExitSuccess && dec == ExitSuccess && fec == ExitSuccess
           then " successfully."
           else " with errors.") <> " Log at " <> tempfile
  hPutStrLn stderr message
  _ <- readProcessWithExitCode "osascript"
    [ "-e"
    , "display notification " <> show message <> " with title "
       <> show "backup-to-storagebox" ] []
  exitWith $ maximum [ bec, dec, fec ]

extractSnapshots :: String -> (Maybe String, Maybe String)
extractSnapshots out = do
  let ls = lines out
  foldl' go (Nothing, Nothing) ls
 where
   go :: (Maybe String, Maybe String) -> String -> (Maybe String, Maybe String)
   go (Just x, Just y) _ = (Just x, Just y)
   go (Nothing, mby) s =
     if "using parent snapshot " `isPrefixOf` s
        then (Just (drop 22 s), mby)
        else (Nothing, mby)
   go (Just x, Nothing) s =
     if "snapshot " `isPrefixOf` s
        then (Just x, Just (takeWhile (/= ' ') (drop 9 s)))
        else (Just x, Nothing)

getResticPath :: IO FilePath
getResticPath = do
  let brewPath = "/opt/homebrew/bin"
  path <- getEnv "PATH"
  setEnv "PATH" (path ++ ":" ++ brewPath)
  mbrestic <- findExecutable "restic"
  case mbrestic of
     Just x -> pure x
     Nothing -> do
       hPutStrLn stderr "restic not found in path"
       exitWith $ ExitFailure 1

