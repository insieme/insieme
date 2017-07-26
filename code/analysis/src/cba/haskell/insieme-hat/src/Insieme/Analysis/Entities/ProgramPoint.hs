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

module Insieme.Analysis.Entities.ProgramPoint where

import Control.DeepSeq
import GHC.Generics (Generic)
import Insieme.Inspire.NodeAddress
import qualified Data.Hashable as Hash


-- The execution phase of an expresssion
data Phase = Pre | Internal | Post 
    deriving (Eq, Ord, Show, Generic, NFData)

instance Hash.Hashable Phase where
    hashWithSalt s Pre      = Hash.hashWithSalt s (  5 :: Int )
    hashWithSalt s Internal = Hash.hashWithSalt s (  7 :: Int )
    hashWithSalt s Post     = Hash.hashWithSalt s ( 11 :: Int )



-- A point in the execution of a program
data ProgramPoint = ProgramPoint NodeAddress Phase
    deriving (Eq, Ord, Show, Generic, NFData)

instance Hash.Hashable ProgramPoint where
    hashWithSalt s (ProgramPoint n p) = Hash.hashWithSalt (Hash.hashWithSalt s n) p
