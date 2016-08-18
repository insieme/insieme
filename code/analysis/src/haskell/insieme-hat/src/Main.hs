{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.List
import Data.Tree (Tree(Node))
import Debug.Trace
import Insieme.Analysis.Entities.FieldIndex (SimpleFieldIndex)
import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Utils as Utils
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet
import qualified Insieme.Utils.UnboundSet as USet



main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right ir = BinPar.parseBinaryDump dump

    let res = Utils.foldTree go ir

    print $ length $ filter (=="error") res
    print $ length $ filter (=="ok") res

 where
    go :: Addr.NodeAddress -> [String] -> [String]
    go addr xs = case Addr.getNode addr of

        Node IR.CallExpr _ | Addr.isBuiltin (Addr.goDown 1 addr) "ref_deref" ->
            if USet.null res
               then "error" : xs
               else "ok" : xs

        _ -> xs

      where
        res :: USet.UnboundSet (Ref.Reference SimpleFieldIndex)
        res = ComposedValue.toValue $ Solver.resolve (Ref.referenceValue $ Addr.goDown 2 addr)
