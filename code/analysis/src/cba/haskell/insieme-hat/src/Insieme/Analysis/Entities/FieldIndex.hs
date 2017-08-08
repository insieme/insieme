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

    -- a type class for field indices
    FieldIndex,
    join,
    project,
    field,
    index,
    element,
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
        {-# MINIMAL join, project, field, index, unknownIndex #-}
        join :: [v] -> [v] -> Maybe [v]
        project :: [v] -> v -> [v]

        field        :: String -> v
        
        index        :: SymbolicFormula -> v
        
        element :: SymbolicFormula -> v
        element = index
        
        unknownIndex :: v

        component :: Int32 -> v
        component = element . mkConst . CInt32
        


-- A simple field index example --

data SimpleFieldIndex =
              Field String
            | Index Int
            | Element Int
            | UnknownIndex
    deriving(Eq,Ord,Typeable,Generic,NFData)

instance Show SimpleFieldIndex where
    show (Field s)    = s
    show (Index i)    = "[" ++ (show i) ++ "]"
    show (Element i)  = "<" ++ (show i) ++ ">"
    show UnknownIndex = "[*]"


-- | Merges the list of simple field indices of the two given lists
simpleJoin :: [SimpleFieldIndex] -> [SimpleFieldIndex] -> Maybe [SimpleFieldIndex]
simpleJoin a b | (allFields a && allFields b) || (allIndices a && allIndices b) || (allElements a && allElements b) =
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

    element a = case toConstant a of
        Just i  -> Element (fromIntegral i)
        Nothing -> UnknownIndex

    unknownIndex = UnknownIndex


isField :: SimpleFieldIndex -> Bool
isField (Field _) = True
isField _         = False

allFields :: [SimpleFieldIndex] -> Bool
allFields = all isField

isIndex :: SimpleFieldIndex -> Bool
isIndex (Index _ ) = True
isIndex _          = False

allIndices :: [SimpleFieldIndex] -> Bool
allIndices = all isIndex

isElement :: SimpleFieldIndex -> Bool
isElement (Element _ ) = True
isElement _            = False

allElements :: [SimpleFieldIndex] -> Bool
allElements = all isElement