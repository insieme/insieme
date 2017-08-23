module Insieme.Benchmark.Utils where

import Control.Exception
import Control.Monad
import System.Exit
import System.Process

inspireBinary :: FilePath
inspireBinary = "inspire"

inspireExists :: IO Bool
inspireExists = handle (const (return False) :: IOException -> IO Bool) $ do
    readProcessWithExitCode inspireBinary ["-v"] ""
    return True

ensureInspire :: IO ()
ensureInspire = do
    exists <- inspireExists
    when (not exists) $ die "inspire binary not found, include it in PATH"

haskellDumper :: FilePath
haskellDumper = "haskell_dumper"

haskellDumperExists :: IO Bool
haskellDumperExists = handle (const (return False) :: IOException -> IO Bool) $ do
    readProcessWithExitCode haskellDumper ["-v"] ""
    return True

ensureHaskellDumper :: IO ()
ensureHaskellDumper = do
    exists <- haskellDumperExists
    when (not exists) $ die "haskell dumper not found, include it in PATH"
