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
import Insieme.TreeUtils
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
-- * Generic Data Flow Value Analysis
--


addrIdent :: NodeAddress -> Solver.Identifier
addrIdent = Solver.mkIdentifier . prettyShow


dataflowValue :: (Solver.Lattice a)
         => NodeAddress
         -> a
         -> (NodeAddress -> Solver.TypedVar a)
         -> Solver.TypedVar a
dataflowValue addr top analysis = case getNode addr of

    Node IR.Declaration _ -> var
      where
        var = Solver.mkVariable (addrIdent addr) [con] Solver.bot
        con = Solver.forward (analysis (goDown 1 addr)) var

    Node IR.Variable _ -> case findDeclr addr of
        Just declrAddr -> handleDeclr declrAddr
        _              -> Solver.mkVariable (addrIdent addr) [] top

    Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable (addrIdent addr) [con,con2] Solver.bot
        con = Solver.createConstraint dep val var
        
        trg = callableValue (goDown 1 addr)
        dep a = (Solver.toVar trg) : (map Solver.toVar (getReturnVars a))
        val a = Solver.join $ map (Solver.get a) (getReturnVars a) 
        getReturnVars a = Set.fold (\c l -> (map (\a -> analysis a) (getReturnPoints c)) ++ l ) [] (Solver.get a trg)

        -- temporary fix to support ref_deref before supporting referencences
        con2 = Solver.createConstraint dep2 val2 var
        isDeref = case getNode (goDown 1 addr) of 
            Node IR.Literal [_, Node (IR.StringValue "ref_deref") _  ] -> True
            _ -> False
        dep2 a = if isDeref then [Solver.toVar trgRef] else [] 
        val2 a = if isDeref then (Solver.get a trgRef) else Solver.bot  
        trgRef = analysis $ goDown 2 addr
        -- end temporary fix

    _ -> Solver.mkVariable (addrIdent addr) [] top
    
  where

    handleDeclr declrAddr = case getNode (goUp declrAddr) of
    
        Node IR.DeclarationStmt _ -> var
          where
            var = Solver.mkVariable (addrIdent addr) [constraint] Solver.bot
            constraint = Solver.forward (analysis (goDown 0 . goUp $ declrAddr)) var
            
        Node IR.Parameters _ -> var
          where 
            var = Solver.mkVariable (addrIdent addr) [con] Solver.bot
            con = Solver.createConstraint dep val var
            
            n = getIndex declrAddr
            
            allCalls = foldTree collector (getRoot declrAddr) 
            collector a calls = case getNode a of
                Node IR.CallExpr _  -> a : calls
                _                   -> calls 
            
            allTrgVars = map (\c -> (c , callableValue $ goDown 1 c ) ) allCalls           
            
            callableAddr = goUp $ goUp declrAddr
            callable = case getNode callableAddr of
                Node IR.Lambda _   -> Callable.Lambda callableAddr
                Node IR.BindExpr _ ->  Callable.Closure callableAddr
            
            dep a = (map Solver.toVar (map snd allTrgVars)) ++ (map Solver.toVar (getArgumentVars a)) 
            val a = Solver.join $ map (Solver.get a) (getArgumentVars a)
            getArgumentVars a = foldr go [] allTrgVars
                where
                    go = \(call,var) list ->  
                            if (Set.member callable (Solver.get a var)) 
                            then (analysis $ goDown (n+2) call) : list 
                            else list
                
            
        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"


getReturnPoints :: Callable.Callable -> [NodeAddress]
getReturnPoints (Callable.Closure x ) = [goDown 2 x] 
getReturnPoints (Callable.Literal x ) = [] --trace "Inspecting a literal" $ error "No return point in literals implemented!"
getReturnPoints (Callable.Lambda x ) = collectReturnPoints x


collectReturnPoints :: NodeAddress -> [NodeAddress]
collectReturnPoints = foldAddressPrune collector filter
    where
        filter cur = 
            case getNode cur of
                Node IR.LambdaExpr _ -> True
                _ -> False
        collector cur returns =
            case getNode cur of 
                Node IR.ReturnStmt _ -> (goDown 0 cur : returns)
                _ -> returns
        



--
-- * Boolean Value Analysis
--

booleanValue :: NodeAddress -> Solver.TypedVar Boolean.Result
--booleanValue a | trace ("Resolving boolean value for " ++ (prettyShow a ) ) False = undefined
booleanValue addr = case getNode addr of
    Node IR.Literal [_, Node (IR.StringValue "true") _] ->
        Solver.mkVariable (addrIdent addr) [] Boolean.AlwaysTrue

    Node IR.Literal [_, Node (IR.StringValue "false") _] ->
        Solver.mkVariable (addrIdent addr) [] Boolean.AlwaysFalse

    _ -> dataflowValue addr Boolean.Both booleanValue


checkBoolean :: NodeAddress -> Boolean.Result
checkBoolean addr = Solver.resolve $ booleanValue addr





--
-- * Callable Analysis
--

callableValue :: NodeAddress -> Solver.TypedVar Callable.CallableSet
--callableValue a | trace ("Resolving callable value for " ++ (prettyShow a ) ) False = undefined
callableValue addr = case getNode addr of
    Node IR.LambdaExpr _ ->
        Solver.mkVariable (addrIdent addr) [] (Set.singleton (Callable.Lambda (fromJust $ getLambda addr )))

    Node IR.BindExpr _ ->
        Solver.mkVariable (addrIdent addr) [] (Set.singleton (Callable.Closure addr))

    Node IR.Literal _ ->
        Solver.mkVariable (addrIdent addr) [] (Set.singleton (Callable.Literal addr))

    _ -> dataflowValue addr allCallables callableValue

  where
    allCallables = Set.fromList $ foldTree collector (getRoot addr)
    collector cur callables = case getNode cur of
        Node IR.Lambda _   -> ((Callable.Lambda  cur) : callables)
        Node IR.BindExpr _ -> ((Callable.Closure cur) : callables)
        Node IR.Literal _  -> ((Callable.Literal cur) : callables)
        _ -> callables  



getLambda :: NodeAddress -> Maybe NodeAddress
getLambda addr =
    case getNode addr of
        Node IR.LambdaExpr [_, ref, Node IR.LambdaDefinition defs] ->
            findLambda ref defs >>= walk addr
        _ -> Nothing
  where
    findLambda ref defs = findIndex ((ref==) . (!!0) . subForest) defs
    walk addr x = Just . goDown 1 . goDown x . goDown 2 $ addr


