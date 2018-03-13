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

{-# LANGUAGE NamedFieldPuns #-}

module Insieme.Solver.Identifier where

import Prelude hiding (lookup,print)
import Data.Dynamic
import Data.Function
import Data.Hashable

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS


import Insieme.Inspire (NodeAddress)

import Insieme.Analysis.Entities.Memory (MemoryStatePoint(..))
import Insieme.Analysis.Entities.ProgramPoint (ProgramPoint(..))

-- * Analysis Identifier -----

data AnalysisIdentifier = AnalysisIdentifier {
    aidToken :: TypeRep,
    aidName  :: String,
    aidHash  :: Int
}

instance Eq AnalysisIdentifier where
    (==) = (==) `on` aidToken

instance Ord AnalysisIdentifier where
    compare = compare `on` aidToken

instance Show AnalysisIdentifier where
    show = aidName

mkAnalysisIdentifier :: (Typeable a) => a -> String -> AnalysisIdentifier
mkAnalysisIdentifier a n = AnalysisIdentifier
    { aidToken = (typeOf a)
    , aidName = n
    , aidHash = (hash n)
    }

-- * Identifier -----

data Identifier = Identifier {
    idHash   :: Int,
    idValue  :: IdentifierValue,
    analysis :: AnalysisIdentifier
} deriving (Eq, Ord)


instance Show Identifier where
        show (Identifier a v _) = (show a) ++ "/" ++ (show v)

instance Hashable Identifier  where
    hashWithSalt s Identifier {idHash} = hashWithSalt s idHash
    hash Identifier {idHash} = idHash

data IdentifierValue =
          IDV_NodeAddress      NodeAddress
        | IDV_ProgramPoint     ProgramPoint
        | IDV_MemoryStatePoint MemoryStatePoint
        | IDV_Other            ByteString
    deriving (Eq,Ord,Show)

referencedAddress :: IdentifierValue -> Maybe NodeAddress
referencedAddress (IDV_NodeAddress   a )                                           = Just a
referencedAddress (IDV_ProgramPoint (ProgramPoint a _) )                           = Just a
referencedAddress (IDV_MemoryStatePoint (MemoryStatePoint (ProgramPoint a _) _ ) ) = Just a
referencedAddress (IDV_Other _ )                                                   = Nothing

mkIdentifierFromExpression :: AnalysisIdentifier -> NodeAddress -> Identifier
mkIdentifierFromExpression a n = Identifier {
        analysis = a,
        idValue = IDV_NodeAddress n,
        idHash = hashWithSalt (aidHash a) n
    }

mkIdentifierFromProgramPoint :: AnalysisIdentifier -> ProgramPoint -> Identifier
mkIdentifierFromProgramPoint a p = Identifier {
    analysis = a,
    idValue = IDV_ProgramPoint p,
    idHash = hashWithSalt (aidHash a) p
}

mkIdentifierFromMemoryStatePoint :: AnalysisIdentifier -> MemoryStatePoint -> Identifier
mkIdentifierFromMemoryStatePoint a m = Identifier {
    analysis = a,
    idValue = IDV_MemoryStatePoint m,
    idHash = hashWithSalt (aidHash a) m
}

mkIdentifierFromString :: AnalysisIdentifier -> String -> Identifier
mkIdentifierFromString a s = Identifier {
    analysis = a,
    idValue = IDV_Other $ BS.pack s,
    idHash = hashWithSalt (aidHash a) s
}

address :: Identifier -> Maybe NodeAddress
address = referencedAddress . idValue
