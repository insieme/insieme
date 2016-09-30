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

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.RecursiveLambdaReferences (

    LambdaReferenceSet,
    recursiveCalls

) where

import Data.Typeable
import qualified Data.Set as Set

import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

import Insieme.Analysis.Solver
import Insieme.Analysis.FreeLambdaReferences
import Insieme.Inspire.Utils


--
-- * RecursiveLambdaReferences Analysis
--

data RecursiveLambdaReferenceAnalysis = RecursiveLambdaReferenceAnalysis
    deriving (Typeable)


--
-- * the constraint generator
--

recursiveCalls :: NodeAddress -> TypedVar LambdaReferenceSet
recursiveCalls addr = case getNodeType addr of
    
        IR.Lambda | depth addr >= 3 -> var
        
        _ -> error "Can only compute recursive calls for lambdas with sufficient context!"

    where
    
        var = mkVariable varId [con] Set.empty
        con = createConstraint dep val var
    
        varId = mkIdentifierFromExpression analysis addr
        analysis = mkAnalysisIdentifier RecursiveLambdaReferenceAnalysis "RecLambdaRefs" 
        
        dep _ = toVar <$> freeRefVars
        val a = Set.filter f $ join $ (get a) <$> freeRefVars
            where
                f r = getNode r == tag 
    
        tag = getNode $ goDown 0 $ goUp addr
        def = goUp $ goUp addr
        
        lambdas = goDown 1 <$> getChildren def
    
        freeRefVars = freeLambdaReferences <$> lambdas    

        
