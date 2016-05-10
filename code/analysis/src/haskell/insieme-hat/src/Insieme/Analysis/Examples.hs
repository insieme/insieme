{-# LANGUAGE PatternGuards #-}

module Insieme.Analysis.Examples where

import Data.List
import Data.Maybe
import Data.Tree
import Insieme.Inspire.NodeAddress
import qualified Debug.Trace as Dbg
import qualified Insieme.Inspire as IR

-- Debug switch
trace = if False then Dbg.trace else flip const

findDeclr :: NodeAddress -> Tree IR.Inspire -> Maybe NodeAddress
findDeclr start tree = findDeclr start
  where
    org = fromJust $ resolve start tree
    findDeclr addr

        -- Check Declaration Statement
        | Just (Node IR.DeclarationStmt [v, _]) <- resolve addr tree
        , v == org
        = trace ("--> Declr found: " ++ show addr) $
          Just $ goDown 0 $ addr

        -- Checking Lambda
        | Just (Node IR.Lambda [_, Node IR.Parameters ps, _]) <- resolve (goUp addr) tree
        = trace ("checking lambda: " ++ show addr) $
          case findIndex (==org) ps of
              Just i  -> Just $ goDown i $ goDown 1 $ goUp $ addr
              Nothing -> findDeclr $ goUp $ addr

        -- Checking For Statement
        | Just (Node IR.ForStmt _) <- resolve (goUp addr) tree
        , _ :>: x <- addr
        , x /= 0
        = trace ("checking for   : " ++ show addr) $
          findDeclr $ goDown 0 . goUp $ addr

        -- Advance to next Statement in Compound Statement
        | Just (Node IR.CompoundStmt _) <- resolve (goUp addr) tree
        , _ :>: x <- addr
        , x /= 0
        = trace ("going left from: " ++ show addr) $
          findDeclr $ goLeft $ addr

        -- Nowhere left to go
        | Empty <- addr
        = trace "no declr found" $ Nothing

        -- Go up for all other Nodes
        | Just _ <- resolve addr tree
        = trace ("going up   from: " ++ show addr) $
          findDeclr $ goUp $ addr

        | otherwise
        = error "fix your shit"
