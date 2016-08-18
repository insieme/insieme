{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.List
import Data.Tree (Tree(Node))
import Debug.Trace
import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Utils as Utils
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet



main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right ir = BinPar.parseBinaryDump dump

    print . length . IR.getTree $ ir
