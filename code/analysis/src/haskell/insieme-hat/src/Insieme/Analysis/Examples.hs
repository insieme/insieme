{-# LANGUAGE LambdaCase #-}

module Insieme.Analysis.Examples where

import Framework
import Compiler.Analysis
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Tree
import Debug.Trace
import Insieme.Inspire.NodeAddress as Addr
import qualified Data.Map as Map
import qualified Insieme.Boolean as Boolean
import qualified Insieme.Inspire as IR

--
-- * Get Definition Point Analysis
--

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

--
-- * Boolean Value Analysis
--

dataflowValue :: Tree IR.Inspire
              -> NodeAddress
              -> (Tree IR.Inspire -> NodeAddress -> State Int AVar)
              -> Constr AVar
              -> (AVar -> Constr AVar)
              -> State Int AVar
dataflowValue tree addr fun top forward = do
    count <- get
    case resolve addr tree of
        Just (Node IR.Literal _) ->
            return $ AVar count (\_ -> top)

        Just (Node IR.Declaration _) -> do
            modify (+1)
            f <- fun tree (goDown 1 addr)
            return $ AVar count (\_ -> forward f)

        Just (Node IR.Variable _) -> do
            case findDeclr addr tree of
                Just declrAddr -> handleDeclr declrAddr
                _ -> return $ AVar count (\_ -> top)

        _ -> return $ AVar count (\_ -> top)

  where
    handleDeclr :: NodeAddress -> State Int AVar
    handleDeclr declrAddr | declrAddr == addr = do
        count <- get
        modify (+1)
        f <- fun tree declrAddr
        return $ AVar count (\_ -> forward f)

    handleDeclr declrAddr = do
        count <- get
        case resolve (goUp declrAddr) tree of
            Just (Node IR.DeclarationStmt _) -> do
                modify (+1)
                f <- fun tree (goDown 0 . goUp $ declrAddr)
                return $ AVar count (\_ -> forward f)
            _ -> error "unhandled case"

boolvalue :: Tree IR.Inspire -> NodeAddress -> State Int AVar
boolvalue tree addr = do
    count <- get
    case resolve addr tree of
        Just (Node IR.Literal [_, Node (IR.StringValue "true") _]) ->
            return $ AVar count (\_ -> constr Nothing [] Boolean.AlwaysTrue)

        Just (Node IR.Literal [_, Node (IR.StringValue "false") _]) ->
            return $ AVar count (\_ -> constr Nothing [] Boolean.AlwaysFalse)

        _ -> modify (+1) >> dataflowValue tree addr boolvalue end forward

  where
    forward :: AVar -> Constr AVar
    forward pred = constr Nothing [pred] (id :: Boolean.Result -> Boolean.Result)

    end :: Constr AVar
    end = constr Nothing [] Boolean.Both


checkBoolean :: NodeAddress -> Tree IR.Inspire -> Boolean.Result
checkBoolean addr tree = resolve' target Boolean.Both
  where
    target = evalState (boolvalue tree addr) 0
