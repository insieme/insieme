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