{-# LANGUAGE LambdaCase #-}

module Insieme.Analysis.Examples where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Tree
import Insieme.Inspire.NodeAddress as Addr
import qualified Data.Map as Map
import qualified Insieme.Inspire as IR

type NodeCache = Map.Map NodeAddress (Tree IR.Inspire)

findDeclr :: NodeAddress -> Tree IR.Inspire -> Maybe NodeAddress
findDeclr start tree = evalState (findDeclr start) Map.empty
  where
    org = fromJust $ resolve start tree

    resolvememo :: NodeAddress -> State NodeCache (Maybe (Tree IR.Inspire))
    resolvememo addr = do
        memo <- get
        case Map.lookup addr memo of
          Just node -> return $ Just node
          Nothing   -> do
              case resolve addr tree of
                Nothing   -> return $ Nothing
                Just node -> do
                    modify (Map.insert addr node)
                    return $ Just node

    findDeclr :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    findDeclr addr = resolvememo addr >>= \case
        Just (Node IR.Lambda _) -> lambda addr
        _ -> declstmt addr <||> forstmt addr <||> bindexpr addr <||>
             compstmt addr <||> nextlevel addr

    declstmt :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    declstmt addr = resolvememo addr >>= return . \case
        Just (Node IR.DeclarationStmt [_, v]) ->
            if v == org
               then Just $ goDown 1 $ addr
               else Nothing
        _ -> Nothing

    forstmt :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    forstmt addr = resolvememo addr >>= \case
        Just (Node IR.ForStmt _) -> declstmt $ goDown 0 $ addr
        _ -> return Nothing

    lambda :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    lambda addr = resolvememo addr >>= return . \case
        Just (Node IR.Lambda [_, Node IR.Parameters ps, _]) ->
            (\i -> goDown i $ goDown 1 $ addr) <$> findIndex (==org) ps
        _ -> Nothing

    bindexpr :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    bindexpr addr = resolvememo addr >>= return . \case
        Just (Node IR.BindExpr [_, Node IR.Parameters ps, _]) ->
            (\i -> goDown i $ goDown 1 $ addr) <$> findIndex (==org) ps
        _ -> Nothing

    compstmt :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    compstmt addr = resolvememo (goUp addr) >>= \case
        Just (Node IR.CompoundStmt _) ->
            case addr of
                _ :>: 0 -> return Nothing
                _ :>: _ -> findDeclr $ goLeft $ addr
                _       -> return Nothing
        _ -> return Nothing

    nextlevel :: NodeAddress -> State NodeCache (Maybe NodeAddress)
    nextlevel addr =
        if Addr.null addr
           then return Nothing
           else findDeclr $ goUp $ addr

(<||>) :: State NodeCache (Maybe NodeAddress)
       -> State NodeCache (Maybe NodeAddress)
       -> State NodeCache (Maybe NodeAddress)
(<||>) = liftM2 (<|>)
infixl 3 <||>
