
module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(DataFlowAnalysis,analysisID,variableGenerator,topValue),
    mkVarIdentifier,
    dataflowValue
) where


import Data.Foldable
import Data.List
import Data.Tree
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set

import qualified Insieme.Inspire as IR
import Insieme.Inspire.Utils
import Insieme.Inspire.NodeAddress

import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.CallSite as CallSite
import qualified Insieme.Analysis.ExitPoint as ExitPoint
import qualified Insieme.Analysis.Reference as Reference

import Insieme.Analysis.Reachable

import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Framework.MemoryState


--
-- * Data Flow Analysis summary
--

data DataFlowAnalysis a = DataFlowAnalysis {
    analysisID         :: String,                               -- ^ an identifier for the represented analysis
    variableGenerator  :: NodeAddress -> Solver.TypedVar a,     -- ^ the variable generator of the represented analysis
    topValue           :: a                                     -- ^ the top value of this analysis
}

-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a -> NodeAddress -> Solver.Identifier
mkVarIdentifier a n = Solver.mkIdentifier $ (analysisID a) ++ (prettyShow n)


--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (Solver.Lattice a)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis a                              -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
dataflowValue addr analysis ops = case getNode addr of

    Node IR.Variable _ -> case findDecl addr of
        Just declrAddr -> handleDeclr declrAddr
        _              -> Solver.mkVariable (idGen addr) [] top

    Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        con = Solver.createConstraint dep val var
        
        trg = Callable.callableValue (goDown 1 addr)
        dep a = (Solver.toVar trg) : ( 
                    (map Solver.toVar (getExitPointVars a)) ++ 
                    (map Solver.toVar (getReturnValueVars a)) ++ 
                    (getOperatorDependencies a)
                )
        val a = Solver.join $ (map (Solver.get a) (getReturnValueVars a)) ++ (getOperatorValue a) 
        
        
        -- support for calls to lambda and closures --
        
        getExitPointVars a = Set.fold go [] (Solver.get a trg)
            where
                go c l = (ExitPoint.exitPoints $ Callable.toAddress c) : l
                
        getReturnValueVars a = foldr go [] $ map (Solver.get a) (getExitPointVars a)
            where 
                go e l = foldr (\(ExitPoint.ExitPoint r) l -> varGen r : l) l e
        
        
        -- operator support --
        
        getActiveOperators a = filter f extOps
            where 
                f o = any (\l -> covers o (Callable.toAddress l)) literals
                literals = Solver.get a trg
        
        getOperatorDependencies a = concat $ map go $ getActiveOperators a
            where
                go o = dependsOn o a
        
        getOperatorValue a = map go $ getActiveOperators a
            where
                go o = getValue o a

        -- TODO: if there is a call to any unintercepted literal, add top to the result



    decl@(Node IR.Declaration _) -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        
        con = Solver.forward 
            (
                if Reference.isMaterializing decl
                then memoryStateValue (MemoryState (ProgramPoint addr Post) (MemoryLocation addr)) analysis
                else varGen (goDown 1 addr)
            ) 
            var


    _ -> Solver.mkVariable (idGen addr) [] top
    
  where
  
    top = topValue analysis
    
    idGen = mkVarIdentifier analysis
    
    varGen = variableGenerator analysis
  

    handleDeclr declrAddr = case getNode (goUp declrAddr) of
    
        Node IR.DeclarationStmt _ -> var
          where
            var = Solver.mkVariable (idGen addr) [constraint] Solver.bot
            constraint = Solver.forward (varGen (goDown 0 . goUp $ declrAddr)) var
            
        Node IR.Parameters _ -> var
          where 
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var
            
            n = getIndex declrAddr
            
            callSiteVar = CallSite.callSites (goUp $ goUp declrAddr)
            
            dep a = (Solver.toVar callSiteVar) : (map Solver.toVar (getArgumentVars a)) 
            val a = Solver.join $ map (Solver.get a) (getArgumentVars a)
            
            getArgumentVars a = foldr go [] $ Solver.get a callSiteVar
                where 
                    go = \(CallSite.CallSite call) list -> (varGen $ goDown (n+2) call) : list
                
            
        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"

    -- add support for predefined operator handlers --
    
    extOps = readHandler : ops
    
    -- support the ref_deref operation (read)
    
    readHandler = OperatorHandler cov dep val
        where 
            cov a = isBuiltin a "ref_deref"
            
            dep a = (Solver.toVar targetRefVar) : (map Solver.toVar $ readValueVars a)
            
            val a = Solver.join $ map (Solver.get a) (readValueVars a)
            
            targetRefVar = Reference.referenceValue $ goDown 1 $ goDown 2 addr          -- here we have to skip the potentially materializing declaration!
            
            readValueVars a = foldr go [] $ Solver.get a targetRefVar
                where
                    go r l = (memoryStateValue (MemoryState (ProgramPoint addr Internal) (MemoryLocation $ Reference.creationPoint r)) analysis) : l
             
    
    
            
