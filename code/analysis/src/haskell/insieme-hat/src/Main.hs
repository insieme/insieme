module Main where

import qualified Data.ByteString as BS
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar

main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right tree = BinPar.parseBinaryDump dump

    print . length . IR.getTree $ tree
