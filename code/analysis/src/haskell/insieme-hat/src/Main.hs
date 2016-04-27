{-# LANGUAGE PatternSynonyms #-}

module Main where

import qualified Data.Sequence as Seq
import Data.Tree
import Insieme.Analysis.Examples
import Text.Show.Pretty
import qualified Data.ByteString as BS
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar

printStringValues :: Tree IR.Inspire -> IO (Tree ())
printStringValues tree = traverse go tree
  where
    go :: IR.Inspire -> IO ()
    go (IR.StringValue s) = print s
    go _ = return ()

main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right tree = BinPar.parseBinaryDump dump

    print $ length tree

    let start = Seq.fromList [0,3,0,1,2,0,1,2,0,1,1]
    let decl = findDeclr start tree
    print decl

    -- printStringValues tree

    -- putStrLn $ ppShow $ children tree

    return ()
