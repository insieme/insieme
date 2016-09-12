{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

{-# LANGUAGE LambdaCase #-}

module Insieme.Inspire.Utils (
    collectAddr,
    foldTree,
    foldAddress,
    foldTreePrune,
    foldAddressPrune,
    parseIR,
    findDecl,
    isLoopIterator,
    getType,
    isType,
    isFreeVariable,
    isEntryPoint,
    isEntryPointParameter
) where

import Control.Applicative
import Data.List
import Data.Maybe
import Insieme.Inspire.BinaryParser
import Insieme.Inspire.NodeAddress
import System.Process
import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Inspire as IR

-- | Collect all nodes of the given 'IR.NodeType' but prune the tree
-- when encountering one of the other 'IR.NodeType's.
collectAddr :: IR.NodeType -> [IR.NodeType -> Bool] -> NodeAddress -> [NodeAddress]
collectAddr ty prune = foldAddressPrune cmpAddTy matchPruneTy
  where
    cmpAddTy n = ([n | getNodeType n == ty] ++)
    matchPruneTy = or . (\n -> map ($n) prune) . getNodeType

-- | Fold the given 'Tree'. The accumulator function takes the subtree
-- and the address of this subtree in the base tree.
foldTree :: Monoid a => (NodeAddress -> a -> a) -> IR.Inspire -> a
foldTree = flip foldTreePrune noPrune

-- | Fold the given 'Tree'. The accumulator function takes the subtree
-- and the address of this subtree in the base tree.
foldAddress :: Monoid a => (NodeAddress -> a -> a) -> NodeAddress -> a
foldAddress = flip foldAddressPrune noPrune

-- | Disables pruning for 'foldTreePrune'.
noPrune :: NodeAddress -> Bool
noPrune = const False

-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldTreePrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> IR.Inspire                   -- ^ current inspire representation
                -> a                            -- ^ accumulated result
foldTreePrune collect prune ir = foldAddressPrune collect prune (mkNodeAddress [] ir)


-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldAddressPrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> NodeAddress                  -- ^ the root of the fold operation
                -> a                            -- ^ accumulated result
foldAddressPrune collect prune addr = visit addr mempty
  where
    visit base acc = if prune base
                     then acc
                     else collect base $ visitsub base acc
    visitsub base acc = foldr visit acc (subtrees base)
    subtrees addr = [goDown i addr | i <- [0..(numChildren addr) - 1]]

--
-- * Parse IR code
--

-- | Parse a given IR statement using the inspire binary.
parseIR :: String -> IO IR.Inspire
parseIR ircode = do
    irb <- readProcess "inspire" ["-s", "-i", "-", "-k", "-"] ircode
    let Right ir = parseBinaryDump (BS8.pack irb)
    return ir

--
-- * Get Definition Point Analysis
--

findDecl :: NodeAddress -> Maybe NodeAddress
findDecl start = findDecl start
  where
    org = getNodePair start

    findDecl :: NodeAddress -> Maybe NodeAddress
    findDecl addr = case getNodePair addr of
        IR.NT IR.Lambda _ -> lambda addr
        _ -> parameter addr <|> declstmt addr <|> forstmt addr   <|>
             bindexpr addr  <|> compstmt addr <|> nextlevel addr

    parameter :: NodeAddress -> Maybe NodeAddress
    parameter addr = case getNodePair addr of
        IR.NT IR.Parameters _ -> Just start
        _ -> Nothing

    declstmt :: NodeAddress -> Maybe NodeAddress
    declstmt addr = case getNodePair addr of
        IR.NT IR.DeclarationStmt [_, v] | v == org -> Just $ goDown 1 addr
        _ -> Nothing

    forstmt :: NodeAddress -> Maybe NodeAddress
    forstmt addr = case getNodePair addr of
        IR.NT IR.ForStmt _ -> declstmt $ goDown 0 addr
        _ -> Nothing

    lambda :: NodeAddress -> Maybe NodeAddress
    lambda addr = case getNodePair addr of
        IR.NT IR.Lambda [_, IR.NT IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> elemIndex org ps
        _ -> Nothing

    bindexpr :: NodeAddress -> Maybe NodeAddress
    bindexpr addr = case getNodePair addr of
        IR.NT IR.BindExpr [_, IR.NT IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> elemIndex org ps
        _ -> Nothing

    compstmt :: NodeAddress -> Maybe NodeAddress
    compstmt addr = getNodePair <$> getParent addr >>= \case
        IR.NT IR.CompoundStmt _ | getIndex addr == 0 -> Nothing
        IR.NT IR.CompoundStmt _ -> findDecl $ goLeft addr
        _ -> Nothing

    nextlevel :: NodeAddress -> Maybe NodeAddress
    nextlevel addr = getParent addr >>= findDecl


-- | Returns 'True' if given variable (in declaration) is a loop iterator.
isLoopIterator :: NodeAddress -> Bool
isLoopIterator a = (depth a >= 2) && ((==IR.ForStmt) $ getNodeType $ goUp $ goUp a)


getType :: IR.Tree -> Maybe IR.NodeType
getType (IR.NT IR.Literal         (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.Variable        (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.CallExpr        (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.LambdaExpr      (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.LambdaReference (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.BindExpr        (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.CastExpr        (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.TupleExpr       (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.InitExpr        (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.JobExpr         (t:_)) = Just $ IR.getNodeType t
getType (IR.NT IR.MarkerExpr      (t:_)) = Just $ IR.getNodeType t
getType _ = Nothing


-- | Return 'True' if the given node is a type.
isType :: IR.NodeType -> Bool
isType = (IR.Type==) . IR.toNodeKind

isVariable :: NodeAddress -> Bool
isVariable = (==IR.Variable) . getNodeType

isFreeVariable :: NodeAddress -> Bool
isFreeVariable v | (not . isVariable) v = False
isFreeVariable v = isNothing (findDecl v)

hasEnclosingStatement :: NodeAddress -> Bool
hasEnclosingStatement a = case getNodePair a of
    IR.NT n _ | (IR.toNodeKind n) == IR.Statement -> True
    IR.NT n _ | n /= IR.LambdaExpr && n /= IR.Variable && (IR.toNodeKind n) == IR.Expression -> True
    _ -> not (isRoot a) && hasEnclosingStatement (fromJust $ getParent a)

isEntryPoint :: NodeAddress -> Bool
isEntryPoint a = case getNodePair a of
    IR.NT IR.CompoundStmt _ -> isRoot a || not (hasEnclosingStatement $ fromJust $ getParent a)
    _                       -> isRoot a

isEntryPointParameter :: NodeAddress -> Bool
isEntryPointParameter v | (not . isVariable) v = False
isEntryPointParameter v | isRoot v = False
isEntryPointParameter v = case getNodePair $ fromJust $ getParent v of
            IR.NT IR.Parameters _ -> not $ hasEnclosingStatement v
            _                     -> False

-- some examples
--excoll a (Node n _) = Set.insert (a, n)
--extree = unfoldTree
--         (\i -> (i, if i>2 then [i `div` 2, i `div` 2 -1] else [])) 8
--exprune s t = Seq.length s <= 2
