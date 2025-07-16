module Main where

import System.Process
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import System.Directory
import Data.Time
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  repo <- case args of
            [x] -> return x
            _ -> do
              hPutStrLn stderr $ "Usage: backup-to-storagebox REPO"
              exitSuccess
  -- Set PATH to include nix profile
  homeDir <- getHomeDirectory
  let nixPath = homeDir </> ".nix-profile" </> "bin"
  path <- getEnv "PATH"
  setEnv "PATH" (nixPath ++ ":" ++ path)
  mbrestic <- findExecutable "restic"
  restic <- case mbrestic of
              Just x -> return x
              Nothing -> do
                hPutStrLn stderr $ "restic not found in path"
                exitWith $ ExitFailure 1
  withSystemTempFile "backup-to-storagebox.XXX" $ \fp h -> do
    hPutStrLn stderr $ "Logging at " ++ fp
    backup restic repo h
    forget restic repo h
    hPutStrLn stderr "Backup succeeded."
    exitWith ExitSuccess

backup :: FilePath -> FilePath -> Handle -> IO ()

backup :: FilePath -> String -> Handle -> IO (String, String)
backup restic repo logh = do
  -- backup
  -- diff
  return ()

forget :: FilePath -> String -> Handle -> IO ()
forget restic repo logh = do
  return ()

{-

  -- Run backup and forget commands
  result <- runBackup homeDir logFile

  -- Extract parent and snapshot IDs from log
  logContent <- readFile logFile
  let parentId = extractParentSnapshot logContent
      snapshotId = extractSnapshot logContent

  -- Run diff command if we have both IDs
  case (parentId, snapshotId) of
    (Just parent, Just snapshot) -> do
      _ <- runDiff homeDir logFile parent snapshot
      return ()
    _ -> return ()

  -- Determine status and create notification
  finalLogContent <- readFile logFile
  let status = determineStatus result finalLogContent
      message = "Backup finished " ++ status ++ " for storagebox:" ++ repo ++ ". Log at " ++ logFile

  putStrLn message
  _ <- runNotification message
  return ()

runBackup :: FilePath -> FilePath -> IO ExitCode
runBackup homeDir logFile = do
  let passwordFile = homeDir </> "Private" </> ("storagebox-" ++ repo ++ ".pwd")
      repoPath = "sftp:storagebox:" ++ repo
      filesFrom = homeDir </> ".config" </> "restic" </> ("files." ++ repo ++ ".lst")
      excludeFile = homeDir </> ".config" </> "restic" </> ("excludes." ++ repo ++ ".lst")

  -- Run backup command
  backupProc <- createProcess (proc "caffeinate" ["-i", "restic", "backup",
    "--password-file", passwordFile,
    "--repo", repoPath,
    "--files-from", filesFrom,
    "--exclude-file", excludeFile,
    "--one-file-system"]) { std_out = UseHandle =<< openFile logFile WriteMode,
                                                   std_err = UseHandle =<< openFile logFile AppendMode }

  backupResult <- waitForProcess (processHandle backupProc)

  case backupResult of
    ExitSuccess -> do
      -- Run forget command
      logHandle <- openFile logFile AppendMode
      forgetProc <- createProcess (proc "caffeinate" ["-i", "restic", "forget",
        "--password-file", passwordFile,
        "--repo", repoPath,
        "--quiet",
        "--prune",
        "--keep-daily", "7",
        "--keep-weekly", "5",
        "--keep-monthly", "12",
        "--keep-yearly", "50"]) { std_out = UseHandle logHandle,
                                   std_err = UseHandle logHandle }

      hClose logHandle
      waitForProcess (processHandle forgetProc)
    _ -> return backupResult

runDiff :: FilePath -> FilePath -> String -> String -> IO ExitCode
runDiff homeDir logFile parentId snapshotId = do
  let passwordFile = homeDir </> "Private" </> ("storagebox-" ++ repo ++ ".pwd")
      repoPath = "sftp:storagebox:" ++ repo

  logHandle <- openFile logFile AppendMode
  diffProc <- createProcess (proc "caffeinate" ["-i", "restic", "diff", parentId, snapshotId,
    "--password-file", passwordFile,
    "--repo", repoPath]) { std_out = UseHandle logHandle,
                           std_err = UseHandle logHandle }

  hClose logHandle
  waitForProcess (processHandle diffProc)

runNotification :: String -> IO ExitCode
runNotification message = do
  (_, _, _, procHandle) <- createProcess (proc "osascript" ["-e",
    "display notification \"" ++ message ++ "\" with title \"restic\""])
  waitForProcess procHandle

extractParentSnapshot :: String -> Maybe String
extractParentSnapshot content =
  case filter ("using parent snapshot" `isInfixOf`) (lines content) of
    (line:_) -> case words line of
      (_:_:_:snapshotId:_) -> Just snapshotId
      _ -> Nothing
    [] -> Nothing
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

extractSnapshot :: String -> Maybe String
extractSnapshot content =
  case filter ("snapshot" `isInfixOf`) (filter ("saved" `isInfixOf`) (lines content)) of
    (line:_) -> case words line of
      (_:snapshotId:_) -> Just snapshotId
      _ -> Nothing
    [] -> Nothing
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

determineStatus :: ExitCode -> String -> String
determineStatus ExitFailure{} _ = "with errors"
determineStatus ExitSuccess content
  | any (\line -> "error:" `isInfixOf` map toLower line || "Error:" `isInfixOf` line) (lines content) = "with errors"
  | any (\line -> "warning" `isInfixOf` map toLower line || "Warning" `isInfixOf` line) (lines content) = "with warnings"
  | otherwise = "successfully"
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c
-}
