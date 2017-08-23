module Insieme.Benchmark.InputData (
    loadAllInputs
  , loadInput
) where

import Control.DeepSeq
import Control.Exception.Base
import Data.ByteString.Char8 (pack)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Insieme.Benchmark.Utils (inspireBinary, haskellDumper)
import Insieme.Inspire.BinaryParser
import System.FilePath.Glob
import System.Process
import Text.Printf

import qualified Data.Map as Map
import qualified Insieme.Inspire as IR

loadAllInputs :: IO (Map FilePath IR.Tree)
loadAllInputs = do
    inputs <- concat <$> fst <$> globDir (compile <$> globs) "bench/data"
    Map.fromList . zip inputs <$> mapM loadInput inputs
  where
    globs = ("**/*."++) <$> validExtensionsIr ++ validExtensionsC

-- | Loads an input program depending on the file extension. The loaded program
-- is fully evaluated.
loadInput :: FilePath -> IO IR.Tree
loadInput p = do
    printf "loading '%s' ... " p

    irbh <- case () of
        _ | ext `elem` validExtensionsIr -> loadIrInput p
          | ext `elem` validExtensionsC  -> loadCInput p
          | otherwise                    -> error $ "unknown extension '" ++ ext ++ "'"

    let Right ir = parseBinaryDump $ pack irbh
    ret <- evaluate $ force ir
    putStrLn "done"
    return ret
  where
    ext = last $ splitOn "." p

validExtensionsIr :: [String]
validExtensionsIr = ["ir"]

validExtensionsC :: [String]
validExtensionsC = ["c", "cc", "cpp", "cxx"]

loadIrInput :: FilePath -> IO String
loadIrInput p = readProcess inspireBinary ["-i", p, "-k", "-"] ""

loadCInput :: FilePath -> IO String
loadCInput p = readProcess haskellDumper ["-i", p, "-d", "-"] ""
