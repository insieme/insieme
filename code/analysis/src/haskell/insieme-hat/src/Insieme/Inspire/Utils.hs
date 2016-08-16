{-# LANGUAGE LambdaCase #-}

module Insieme.Inspire.Utils (
    foldTree,
    foldAddress,
    foldTreePrune,
    foldAddressPrune,
    parseIR,
    findDecl,
    isFreeVariable
) where

import Control.Applicative
import Data.Maybe
import Data.List
import Data.Tree
import Insieme.Inspire.BinaryParser
import Insieme.Inspire.NodeAddress
import System.Process
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR

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
noPrune _ = False

-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldTreePrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> IR.Inspire                   -- ^ initial tree
                -> a                            -- ^ accumulated result
foldTreePrune collect prune tree = foldAddressPrune collect prune (mkNodeAddress [] tree)


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
    subtrees addr = [goDown i addr | i <- [0..(length . subForest . getNode $ addr) - 1]]

--
-- * Parse IR code
--

-- | Parse a given IR statement using the inspire binary.
parseIR :: String -> IO IR.Inspire
parseIR ircode = do
    irb <- readProcess "inspire" ["-s", "-i", "-", "-k", "-"] ircode
    let (Right ir) = parseBinaryDump (BS8.pack irb)
    return ir

--
-- * Get Definition Point Analysis
--

findDecl :: NodeAddress -> Maybe NodeAddress
findDecl start = findDecl start
  where
    org = getNode start

    findDecl :: NodeAddress -> Maybe NodeAddress
    findDecl addr = case getNode addr of
        Node IR.Lambda _ -> lambda addr
        _ -> declstmt addr <|> forstmt addr <|> bindexpr addr <|>
             compstmt addr <|> nextlevel addr

    declstmt :: NodeAddress -> Maybe NodeAddress
    declstmt addr = case getNode addr of
        Node IR.DeclarationStmt [_, v] | v == org -> Just $ goDown 1 addr
        _ -> Nothing

    forstmt :: NodeAddress -> Maybe NodeAddress
    forstmt addr = case getNode addr of
        Node IR.ForStmt _ -> declstmt $ goDown 0 addr
        _ -> Nothing

    lambda :: NodeAddress -> Maybe NodeAddress
    lambda addr = case getNode addr of
        Node IR.Lambda [_, Node IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> findIndex (==org) ps
        _ -> Nothing

    bindexpr :: NodeAddress -> Maybe NodeAddress
    bindexpr addr = case getNode addr of
        Node IR.BindExpr [_, Node IR.Parameters ps, _] ->
            (\i -> goDown i . goDown 1 $ addr) <$> findIndex (==org) ps
        _ -> Nothing

    compstmt :: NodeAddress -> Maybe NodeAddress
    compstmt addr = getNode <$> getParent addr >>= \case
        Node IR.CompoundStmt _ | last (getAddress addr) == 0 -> Nothing
        Node IR.CompoundStmt _ -> findDecl $ goLeft addr
        _ -> Nothing

    nextlevel :: NodeAddress -> Maybe NodeAddress
    nextlevel addr = getParent addr >>= findDecl


isFreeVariable :: NodeAddress -> Bool
isFreeVariable v = isNothing decl || isEntryPointParam decl
    where
        decl = findDecl v
        
        isEntryPointParam (Just v) = case getNode $ fromJust $ getParent v of
            Node IR.Parameters _ -> (not . hasEnclosingCall) v
            _                    -> False
            
        isEntryPointParam Nothing  = False
        
        hasEnclosingCall a = case getNode a of
            Node IR.CallExpr _ -> True
            _                  -> (not . isRoot) a && (hasEnclosingCall (fromJust $ getParent a))




-- some examples
excoll a (Node n _) = Set.insert (a, n)
extree = unfoldTree
         (\i -> (i, if i>2 then [i `div` 2, i `div` 2 -1] else [])) 8
exprune s t = Seq.length s <= 2
