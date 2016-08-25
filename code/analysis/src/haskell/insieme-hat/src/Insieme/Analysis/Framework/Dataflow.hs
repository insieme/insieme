{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Insieme.Analysis.Framework.Dataflow (
    DataFlowAnalysis(DataFlowAnalysis,analysis,analysisIdentifier,variableGenerator,topValue),
    mkVarIdentifier,
    dataflowValue
) where


import Data.Int
import Data.Foldable
import Data.Tree
import Data.Typeable
import Debug.Trace
import qualified Data.Set as Set

import qualified Insieme.Utils.BoundSet as BSet

import qualified Insieme.Inspire as IR
import Insieme.Inspire.Utils
import Insieme.Inspire.NodeAddress

import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Analysis.Callable as Callable
import qualified Insieme.Analysis.CallSite as CallSite
import qualified Insieme.Analysis.ExitPoint as ExitPoint
import qualified Insieme.Analysis.Reference as Reference
import qualified Insieme.Analysis.Arithmetic as Arithmetic

import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.Framework.Utils.OperatorHandler
import Insieme.Analysis.Framework.MemoryState

import qualified Insieme.Utils.UnboundSet as USet
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue

--
-- * Data Flow Analysis summary
--

data DataFlowAnalysis a v = DataFlowAnalysis {
    analysis           :: a,                                    -- ^ the analysis type token
    analysisIdentifier :: Solver.AnalysisIdentifier,            -- ^ the analysis identifier
    variableGenerator  :: NodeAddress -> Solver.TypedVar v,     -- ^ the variable generator of the represented analysis
    topValue           :: v                                     -- ^ the top value of this analysis
}

-- a function creation an identifier for a variable of a data flow analysis
mkVarIdentifier :: DataFlowAnalysis a v -> NodeAddress -> Solver.Identifier
mkVarIdentifier a n = Solver.mkIdentifier (analysisIdentifier a) n ""


--
-- * Generic Data Flow Value Analysis
--

dataflowValue :: (ComposedValue.ComposedValue a i v, Typeable d)
         => NodeAddress                                     -- ^ the address of the node for which to compute a variable representing the data flow value
         -> DataFlowAnalysis d a                            -- ^ the summar of the analysis to be performed be realized by this function
         -> [OperatorHandler a]                             -- ^ allows selected operators to be intercepted and interpreted
         -> Solver.TypedVar a                               -- ^ the resulting variable representing the requested information
dataflowValue addr analysis ops = case getNode addr of


    Node IR.Variable _ -> case findDecl addr of
            Just declrAddr -> if isFreeVariable declrAddr 
                                then freeVar 
                                else handleDeclr declrAddr
            _              -> freeVar
        where
            freeVar = Solver.mkVariable (idGen addr) [] top


    Node IR.CallExpr _ -> var
      where
        var = Solver.mkVariable (idGen addr) [knownTargets,unknownTarget] Solver.bot
        knownTargets = Solver.createConstraint dep val var
        
        trg = Callable.callableValue (goDown 1 addr)
        dep a = (Solver.toVar trg) : ( 
                    (map Solver.toVar (getExitPointVars a)) ++ 
                    (map Solver.toVar (getReturnValueVars a)) ++ 
                    (getOperatorDependencies a)
                )
        val a = Solver.join $ (map (Solver.get a) (getReturnValueVars a)) ++ (getOperatorValue a)
        
        
        -- support for calls to lambda and closures --
        
        getExitPointVars a = if USet.isUniverse targets then [] else Set.fold go [] (USet.toSet $ targets)
            where
                targets = ComposedValue.toValue $ Solver.get a trg
                go c l = (ExitPoint.exitPoints $ Callable.toAddress c) : l
                
        getReturnValueVars a = concat $ map go $ map (toList . Solver.get a) (getExitPointVars a)
            where 
                go e = map resolve e
                
                resolve (ExitPoint.ExitPoint r) = case getNode r of
                    Node IR.Declaration  _ -> memoryStateValue (MemoryState (ProgramPoint r Post) (MemoryLocation r)) analysis
                    _                      -> varGen r
                    
        
        
        -- operator support --
        
        getActiveOperators a = if USet.isUniverse targets then [] else filter f extOps
            where 
                targets = ComposedValue.toValue $ Solver.get a trg
                f o = any (\l -> covers o (Callable.toAddress l)) $ USet.toSet $ targets
        
        getOperatorDependencies a = concat $ map go $ getActiveOperators a
            where
                go o = dependsOn o a
        
        getOperatorValue a = map go $ getActiveOperators a
            where
                go o = getValue o a


        -- support calls to unknown literal --
        
        unknownTarget = Solver.createConstraint dep val var
            where
                dep a = [Solver.toVar trg]
                val a = if hasUnknownTarget then top else Solver.bot
                    where 
                        
                        targets = ComposedValue.toValue $ Solver.get a trg
                        
                        uncoveredLiterals = filter f $ USet.toList $ targets
                            where
                                f (Callable.Literal a) = not $ any (\h -> covers h a) extOps
                                f _                    = False
                         
                        hasUnknownTarget = (USet.isUniverse targets) || ((not . null) uncoveredLiterals)



    Node IR.TupleExpr [_,Node IR.Expressions args] -> var
        where
            var = Solver.mkVariable (idGen addr) [con] Solver.bot
            con = Solver.createConstraint dep val var
            
            dep _ = Solver.toVar <$> componentValueVars
            val a = ComposedValue.composeElements $ zip (component <$> [0 :: Int32 ..]) (map (Solver.get a) componentValueVars) 
            
            componentValueVars = go <$> [0 .. ((length args) - 1) ]
                where
                    go i = varGen (goDown i $ goDown 1 $ addr)
                    

    decl@(Node IR.Declaration _) -> var
      where
        var = Solver.mkVariable (idGen addr) [con] Solver.bot
        
        con = Solver.forward 
            (
                if Reference.isMaterializingDeclaration decl
                then memoryStateValue (MemoryState (ProgramPoint addr Post) (MemoryLocation addr)) analysis
                else varGen (goDown 1 addr)
            ) 
            var


    _ -> Solver.mkVariable (idGen addr) [] top
    
  where
  
    top = topValue analysis
    
    idGen = mkVarIdentifier analysis
    
    varGen = variableGenerator analysis
    
    compose = ComposedValue.toComposed
    extract = ComposedValue.toValue
    

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
            val a = compose $ Solver.join $ map (extract . Solver.get a) (getArgumentVars a)
            
            getArgumentVars a = foldr go [] $ Solver.get a callSiteVar
                where 
                    go = \(CallSite.CallSite call) list -> (varGen $ goDown (n+2) call) : list
                
            
        _ -> trace " Unhandled Variable parent!" $ error "unhandled case"

    -- add support for predefined operator handlers --
    
    extOps = readHandler : tupleMemberAccessHandler : ops
    
    -- support the ref_deref operation (read)
    
    readHandler = OperatorHandler cov dep val
        where 
            cov a = isBuiltin a "ref_deref"
            
            dep a = (Solver.toVar targetRefVar) : (map Solver.toVar $ readValueVars a)
            
            val a = if USet.isUniverse targets then top else Solver.join $ map go $ USet.toList targets 
                where 
                    targets = targetRefVal a
                    go r = ComposedValue.getElement (Reference.dataPath r) $ Solver.get a (memStateVarOf r)
            
            targetRefVar = Reference.referenceValue $ goDown 1 $ goDown 2 addr          -- here we have to skip the potentially materializing declaration!
            targetRefVal a = ComposedValue.toValue $ Solver.get a targetRefVar
            
            readValueVars a = if USet.isUniverse targets then [] else map memStateVarOf $ USet.toList targets
                where
                    targets = targetRefVal a
                    
            memStateVarOf r = memoryStateValue (MemoryState (ProgramPoint addr Internal) (MemoryLocation $ Reference.creationPoint r)) analysis

             
    -- support the tuple_member_access operation (read from tuple component)
    
    tupleMemberAccessHandler  = OperatorHandler cov dep val
        where 
            cov a = isBuiltin a "tuple_member_access"
            
            dep a = Solver.toVar indexValueVar : Solver.toVar tupleValueVar : []
            
            val a = if BSet.isUniverse indices 
                    then top 
                    else Solver.join $ map go dataPaths
                where                    
                    
                    indices = ComposedValue.toValue (Solver.get a indexValueVar)
                    
                    fieldIndices = index <$> (BSet.toList indices)
                    
                    dataPaths = step <$> fieldIndices
                    
                    tupleValue = Solver.get a tupleValueVar
                    
                    go i = ComposedValue.getElement i tupleValue
                    
                    
            indexValueVar = Arithmetic.arithmeticValue $ goDown 3 addr

            tupleValueVar = varGen $ goDown 1 $ goDown 2 addr
            
             
