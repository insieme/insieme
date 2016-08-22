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

module Insieme.Analysis.Entities.FieldIndex (
    
    -- a type class for field indices
    FieldIndex,
    join,
    project,
    field,
    index,
    component,
    
    -- an example implementation
    SimpleFieldIndex
    
) where

import Data.Int
import Data.Typeable
import Insieme.Utils.Arithmetic
import Insieme.Utils.ParseInt
import Insieme.Analysis.Entities.SymbolicFormula
 
import qualified Data.Set as Set

class (Eq v, Ord v, Show v, Typeable v) => FieldIndex v where
        {-# MINIMAL join, project, field, index #-}
        join :: [v] -> [v] -> Maybe [v]
        project :: [v] -> v -> [v]
        
        field :: String -> v
        index :: SymbolicFormula -> v

        component :: Int32 -> v
        component = index . mkConst . CInt32


-- A simple field index example --

data SimpleFieldIndex = 
              Field String
            | Index Int
            | UnknownIndex
    deriving(Eq,Ord,Typeable)
    
instance Show SimpleFieldIndex where
    show (Field s) = s
    show (Index i) = "[" ++ (show i) ++ "]"
    show UnknownIndex = "[*]"
    

-- | Merges the list of simple field indices of the two given lists    
simpleJoin :: [SimpleFieldIndex] -> [SimpleFieldIndex] -> Maybe [SimpleFieldIndex]
simpleJoin a b | (allFields a && allFields b) || (allIndices a && allIndices b) = 
    Just $ Set.toList . Set.fromList $ a ++ b
simpleJoin _ _ = Nothing

-- | Computes the list of indices to combine when accessing a specific indices
simpleProject :: [SimpleFieldIndex] -> SimpleFieldIndex -> [SimpleFieldIndex]
simpleProject is i = if elem i is then [i] else [UnknownIndex]
    
    
instance FieldIndex SimpleFieldIndex where
    join = simpleJoin
    project = simpleProject
    
    field n = Field n
    index a = case toConstant a of 
        Just i  -> Index (fromIntegral i)
        Nothing -> UnknownIndex 


isField :: SimpleFieldIndex -> Bool
isField (Field _) = True
isField _         = False

allFields :: [SimpleFieldIndex] -> Bool
allFields = all isField
        
allIndices :: [SimpleFieldIndex] -> Bool
allIndices = all $ not . isField
                
        
        