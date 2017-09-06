{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
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
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Insieme.Analysis.Entities.FieldIndex (

    -- a type class for field indexes
    FieldIndex,
    join,
    project,
    structField,
    unionField,
    tupleElementIndex,
    arrayIndex,
    unknownIndex,
    component,

    -- an example implementation
    SimpleFieldIndex(..)

) where

import Control.DeepSeq
import Data.Int
import Data.Typeable
import GHC.Generics (Generic)
import Insieme.Analysis.Entities.SymbolicFormula
import Insieme.Utils.Arithmetic
import Insieme.Utils.ParseInt

import qualified Data.Set as Set

class (Eq v, Ord v, Show v, Typeable v, NFData v) => FieldIndex v where
        
        -- interface for integration in composed value trees
        join :: [v] -> [v] -> Maybe [v]
        project :: [v] -> v -> [v]

        -- interface for integration in data path analysis
        
        structField        :: String -> v
        unionField         :: String -> v
        
        tupleElementIndex  :: SymbolicFormula -> v
        
        arrayIndex         :: SymbolicFormula -> v
        
        unknownIndex       :: v


        -- convenience functions
        
        component :: Int32 -> v
        component = tupleElementIndex . mkConst . CInt32
        


-- A simple field index working with numericall values for indices only --


data SimpleFieldIndex =
          StructField String
        | UnionField String

        | TupleElementIndex Int

        | ArrayIndex Int                    -- Index of the array element

--        | StdArrayIndex Int                 -- Index of the std::array element
--        
--        | StdVectorIndex Int Int            -- Index of the std::vector elemnet and version
--        
--        | StdSetIndex Bool                  -- True = Begin, False = End

        | UnknownIndex                        -- the index is not known

    deriving(Eq,Ord,Typeable,Generic,NFData)

instance Show SimpleFieldIndex where
    show (StructField s)       = s
    show (UnionField s)        = s
    show (TupleElementIndex i) = "<" ++ (show i) ++ ">"
    show (ArrayIndex i)        = "[" ++ (show i) ++ "]"
    show  UnknownIndex         = "[*]"
    

instance FieldIndex SimpleFieldIndex where
    join    = simpleJoin
    project = simpleProject

    structField = StructField
    unionField  = StructField

    tupleElementIndex a = case toConstant a of
        Just i  -> TupleElementIndex (fromIntegral i)
        Nothing -> UnknownIndex

    arrayIndex a = case toConstant a of
        Just i  -> ArrayIndex (fromIntegral i)
        Nothing -> UnknownIndex

    unknownIndex = UnknownIndex



-- | Merges the list of simple field indices of the two given lists
simpleJoin :: [SimpleFieldIndex] -> [SimpleFieldIndex] -> Maybe [SimpleFieldIndex]
simpleJoin a b | sameIndexTypes a b =
    Just $ Set.toList . Set.fromList $ a ++ b
simpleJoin _ _ = Nothing

-- | Computes the list of indices to combine when accessing a specific indices
simpleProject :: [SimpleFieldIndex] -> SimpleFieldIndex -> [SimpleFieldIndex]
simpleProject is i = if elem i is then [i] else [UnknownIndex]


-- Utilities:

sameIndexTypes :: [SimpleFieldIndex] -> [SimpleFieldIndex] -> Bool
sameIndexTypes a b =
    (       allStructFields a &&        allStructFields b) ||
    (        allUnionFields a &&         allUnionFields b) ||
    (allTupleElementIndexes a && allTupleElementIndexes b) ||
    (       allArrayIndexes a &&        allArrayIndexes b)
    


isStructField :: SimpleFieldIndex -> Bool
isStructField (StructField _) = True
isStructField _               = False

allStructFields :: [SimpleFieldIndex] -> Bool
allStructFields = all isStructField

isUnionField :: SimpleFieldIndex -> Bool
isUnionField (UnionField _) = True
isUnionField _              = False

allUnionFields :: [SimpleFieldIndex] -> Bool
allUnionFields = all isUnionField

isTupleElementIndex :: SimpleFieldIndex -> Bool
isTupleElementIndex (TupleElementIndex _ ) = True
isTupleElementIndex _                      = False

allTupleElementIndexes :: [SimpleFieldIndex] -> Bool
allTupleElementIndexes = all isTupleElementIndex

isArrayIndex :: SimpleFieldIndex -> Bool
isArrayIndex (ArrayIndex _ ) = True
isArrayIndex _          = False

allArrayIndexes :: [SimpleFieldIndex] -> Bool
allArrayIndexes = all isArrayIndex
