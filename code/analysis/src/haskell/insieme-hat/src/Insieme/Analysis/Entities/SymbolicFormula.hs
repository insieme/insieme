{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Entities.SymbolicFormula where

import Data.Tree
import Insieme.Utils.ParseInt
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.NodeAddress as Addr

--
-- * Arithemtic Symbol
--

data Symbol = Constant {getNode :: (Tree IR.NodeType), getAddr :: Addr.NodeAddress }
            | Variable {getNode :: (Tree IR.NodeType), getAddr :: Addr.NodeAddress }

instance Eq Symbol where
    x == y = (getNode x) == (getNode y)

instance Ord Symbol where
    compare x y = compare (getNode x) (getNode y)

instance Show Symbol where
    show (Constant (Node IR.Literal  [_, Node (IR.StringValue v) _]) _) = v
    show (Variable (Node IR.Variable [_, Node (IR.UIntValue   v) _]) _) = "v" ++ show v
    show _ = "???"

    
type SymbolicFormula = Ar.Formula CInt Symbol