module Insieme.Analysis.Examples where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import Data.Tree
import Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire as IR

findDeclr :: NodeAddress -> Tree IR.Inspire -> Maybe NodeAddress
findDeclr start tree = evalStateT findDeclr start
  where
    org = fromJust $ resolve start tree

    findDeclr :: StateT NodeAddress Maybe NodeAddress
    findDeclr = do
        addr <- get
        case resolve addr tree of
            Just (Node IR.Lambda _) -> lambda
            _ -> declstmt <|> forstmt <|> bindexpr <|> compstmt <|> nextlevel

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

    bindexpr :: StateT NodeAddress Maybe NodeAddress
    bindexpr = do
        addr <- get
        lift $ case resolve addr tree of
            Just (Node IR.BindExpr [_, Node IR.Parameters ps, _]) ->
                (\i -> goDown i $ goDown 1 $ addr) <$> findIndex (==org) ps
            _ -> Nothing

    compstmt :: StateT NodeAddress Maybe NodeAddress
    compstmt = do
        addr <- get
        case resolve (goUp addr) tree of
            Just (Node IR.CompoundStmt _) ->
                case addr of
                    _ :>: 0 -> lift Nothing
                    _ :>: _ -> modify goLeft >> findDeclr
                    _       -> lift Nothing
            _ -> lift Nothing

    nextlevel :: StateT NodeAddress Maybe NodeAddress
    nextlevel = do
        addr <- get
        if Addr.null addr
           then lift Nothing
           else modify goUp >> findDeclr
