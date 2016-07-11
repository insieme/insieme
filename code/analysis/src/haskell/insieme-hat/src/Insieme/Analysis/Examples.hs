{-# LANGUAGE LambdaCase #-}

module Insieme.Analysis.Examples where

-- import Compiler.Analysis
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Tree
import Debug.Trace
import Framework
import Insieme.Inspire.NodeAddress as Addr
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Insieme.Boolean as Boolean
import qualified Insieme.Callable as Callable
import qualified Insieme.Inspire as IR
import qualified Solver

--
-- * Get Definition Point Analysis
--

findDeclr :: NodeAddress -> Maybe NodeAddress
findDeclr start = findDeclr start
  where
    org = getNode start

    findDeclr :: NodeAddress -> Maybe NodeAddress
    findDeclr addr = case getNode addr of
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
        Node IR.CompoundStmt _ -> findDeclr $ goLeft addr
        _ -> Nothing

    nextlevel :: NodeAddress -> Maybe NodeAddress
    nextlevel addr = getParent addr >>= findDeclr

--
-- * Boolean Value Analysis
--

dataflowValue :: (Solver.Lattice a)
         => NodeAddress
         -> a
         -> (NodeAddress -> Solver.TypedVar a)
         -> Solver.TypedVar a
dataflowValue addr top analysis = case getNode addr of
    Node IR.Literal _ -> Solver.mkVariable (addrIdent addr) [] top

    Node IR.Declaration _ -> var
      where
        var = Solver.mkVariable (addrIdent addr) [con] Solver.bot
        con = Solver.forward (analysis (goDown 1 addr)) var

    Node IR.Variable _ -> case findDeclr addr of
        Just declrAddr -> handleDeclr declrAddr
        _              -> Solver.mkVariable (addrIdent addr) [] top

    _ -> Solver.mkVariable (addrIdent addr) [] top
  where
    handleDeclr declrAddr | declrAddr == addr = var
      where
        var = Solver.mkVariable (addrIdent addr) [constraint] Solver.bot
        constraint = Solver.forward (analysis (goDown 1 addr)) var

    handleDeclr declrAddr = case getNode (goUp declrAddr) of
        Node IR.DeclarationStmt _ -> var
          where
            var = Solver.mkVariable (addrIdent addr) [constraint] Solver.bot
            constraint = Solver.forward (analysis (goDown 0 . goUp $ declrAddr)) var
        _ -> error "unhandled case"


booleanValue :: NodeAddress -> Solver.TypedVar Boolean.Result
booleanValue addr = case getNode addr of
    Node IR.Literal [_, Node (IR.StringValue "true") _] ->
        Solver.mkVariable (addrIdent addr) [] Boolean.AlwaysTrue

    Node IR.Literal [_, Node (IR.StringValue "false") _] ->
        Solver.mkVariable (addrIdent addr) [] Boolean.AlwaysFalse

    _ -> dataflowValue addr Boolean.Both booleanValue

checkBoolean :: NodeAddress -> Boolean.Result
checkBoolean addr = Solver.resolve $ booleanValue addr

addrIdent :: NodeAddress -> Solver.Identifier
addrIdent = Solver.mkIdentifier . prettyShow

--
-- * Callable Analysis
--

{-

callableValue :: Tree IR.Inspire -> NodeAddress -> State Int AVar
callableValue tree addr = do
    count <- get
    case resolve addr tree of
        Just (Node IR.LambdaExpr _) ->
            return $ AVar count (\_ -> constr Nothing [] (Set.singleton (Callable.Lambda (fromJust $ getLambda tree addr))))

        Just (Node IR.Literal _) ->
            return $ AVar count (\_ -> constr Nothing [] (Set.singleton (Callable.Literal addr)))

        Just (Node IR.BindExpr _) ->
            return $ AVar count (\_ -> constr Nothing [] (Set.singleton (Callable.Closure addr)))

        _ -> modify (+1) >> dataflowValue tree addr callableValue end forward

  where
    forward :: AVar -> Constr AVar
    forward pred = constr Nothing [pred] (id :: Callable.CallableSet -> Callable.CallableSet)

    end :: Constr AVar
    end = constr Nothing [] (findEverything tree)

getLambda :: Tree IR.Inspire -> NodeAddress -> Maybe NodeAddress
getLambda tree addr =
    case resolve addr tree of
        Just (Node IR.LambdaExpr [_, ref, Node IR.LambdaDefinition defs]) ->
            findLambda ref defs >>= walk addr
        _ -> Nothing
  where
    findLambda ref defs = findIndex ((ref==) . (!!0) . subForest) defs
    walk addr x = Just . goDown 1 . goDown x . goDown 2 $ addr

-- TODO: remove
findEverything :: Tree IR.Inspire -> Callable.CallableSet
findEverything tree = visit tree go' Set.empty

  where

    visit :: Tree IR.Inspire -> (Tree IR.Inspire -> NodeAddress -> a -> a) -> a -> a
    visit tree go s = visit' tree Seq.empty go s

    visit' :: Tree IR.Inspire -> NodeAddress -> (Tree IR.Inspire -> NodeAddress -> a -> a)
           -> a -> a
    visit' tree addr go s = foldr (\(t, i) s' -> go t (goDown i addr) (visit' t (goDown i addr) go s') ) s (Prelude.zip (subForest tree) [0..])

    go' :: Tree IR.Inspire -> NodeAddress -> Callable.CallableSet -> Callable.CallableSet
    go' (Node IR.Lambda   _) addr s = Set.insert (Callable.Lambda  addr) s
    go' (Node IR.Literal  _) addr s = Set.insert (Callable.Literal addr) s
    go' (Node IR.BindExpr _) addr s = Set.insert (Callable.Closure addr) s
    go' _ _ s = s

-}
