{-# LANGUAGE PatternSynonyms #-}

module Insieme.Analysis.Examples where

import Data.Maybe
import Data.Tree
import Debug.Trace
import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

findDeclr :: NodeAddress -> Tree IR.Inspire -> Maybe NodeAddress
findDeclr start tree = findDeclr start tree
  where
    org = fromJust $ resolve start tree
    findDeclr addr tree

        -- Check Declaration Statement
        | Just (Node IR.DeclarationStmt [v, _]) <- resolve addr tree
        , v == org
        = trace ("--> Declr found: " ++ show addr) $ Just $ goDown 0 $ addr

        -- Check Lambda Parameter
        | Just (Node IR.Parameters _) <- resolve (goUp addr) tree
        , Just v <- resolve addr tree
        , v == org
        = trace ("--> Param found: " ++ show addr) $ Just addr

        -- Advance to next Lambda Parameter
        | Just (Node IR.Parameters _) <- resolve (goUp addr) tree
        , _ :>: x <- addr
        , x /= 0
        = trace ("checking param : " ++ show addr) $ findDeclr (goLeft addr) tree

        -- Checking Lambda
        | Just (Node IR.Lambda [_, Node IR.Parameters ps, _]) <- resolve (goUp addr) tree
        , _ :>: x <- addr
        , x /= 1
        = trace ("checking lambda: " ++ show addr) $ findDeclr (goDown (Prelude.length ps - 1) . goDown 1 . goUp $ addr) tree

        -- Checking For Statement
        | Just (Node IR.ForStmt _) <- resolve (goUp addr) tree
        , _ :>: x <- addr
        , x /= 0
        = trace ("checking for   : " ++ show addr) $ findDeclr (goDown 0 . goUp $ addr) tree

        -- Advance to next Statement in Compound Statement
        | Just (Node IR.CompoundStmt _) <- resolve (goUp addr) tree
        , _ :>: x <- addr
        , x /= 0
        = trace ("going left from: " ++ show addr) $ findDeclr (goLeft addr) tree

        -- Nowhere left to go
        | Empty <- addr
        = trace "no declr found" $ Nothing

        -- Go up for all other Nodes
        | Just _ <- resolve addr tree
        = trace ("going up   from: " ++ show addr) $ findDeclr (goUp addr) tree

        | otherwise
        = error "fix your shit"
