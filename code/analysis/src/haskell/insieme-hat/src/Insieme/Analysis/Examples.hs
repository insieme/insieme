{-# LANGUAGE PatternGuards #-}

module Insieme.Analysis.Examples where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Tree
import Insieme.Inspire.NodeAddress as Addr
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
        | Just (Node IR.Lambda [_, Node IR.Parameters ps, _]) <- resolve addr tree
        = trace ("checking lambda: " ++ show addr) $
          case findIndex (==org) ps of
              Just i  -> Just $ goDown i $ goDown 1 $ addr
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

findDeclr2 :: NodeAddress -> Tree IR.Inspire -> Maybe NodeAddress
findDeclr2 start tree = evalStateT findDeclr start
  where
    org = fromJust $ resolve start tree

    findDeclr :: StateT NodeAddress Maybe NodeAddress
    findDeclr = declstmt <|> forstmt <|> lambda <|> compstmt <|> nextlevel

    declstmt :: StateT NodeAddress Maybe NodeAddress
    declstmt = do
        addr <- get
        lift $ case resolve addr tree of
            Just (Node IR.DeclarationStmt [v, _]) ->
                if v == org
                   then Just $ goDown 0 $ addr
                   else Nothing
            _ -> Nothing

    forstmt :: StateT NodeAddress Maybe NodeAddress
    forstmt = do
        addr <- get
        case resolve addr tree of
            Just (Node IR.ForStmt _) ->
                modify (goDown 0) >> declstmt
            _ -> lift Nothing

    lambda :: StateT NodeAddress Maybe NodeAddress
    lambda = do
        addr <- get
        lift $ case resolve addr tree of
            Just (Node IR.Lambda [_, Node IR.Parameters ps, _]) ->
                (\i -> goDown i $ goDown 1 $ addr) <$> findIndex (==org) ps
            _ -> Nothing

    compstmt :: StateT NodeAddress Maybe NodeAddress
    compstmt = do
        addr <- get
        case resolve (goUp addr) tree of
            Just (Node IR.CompoundStmt _) ->
                case addr of
                    _ :>: 0 -> lift Nothing
                    Empty   -> lift Nothing
                    _       -> modify goLeft >> findDeclr
            _ -> lift Nothing

    nextlevel :: StateT NodeAddress Maybe NodeAddress
    nextlevel = do
        addr <- get
        if Addr.null addr
           then lift Nothing
           else modify goUp >> findDeclr
