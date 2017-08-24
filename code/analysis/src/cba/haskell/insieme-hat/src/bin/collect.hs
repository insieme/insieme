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

module Main where

import System.IO
import Control.Monad
import Control.Exception
import Control.DeepSeq
import Data.List
import Insieme.Inspire.BinaryParser
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Visit

import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Insieme.Inspire as IR

main :: IO ()
main = do
    Right ir <- parseBinaryDump <$> BS.getContents
    -- ir' <- evaluate $ force ir

--    print  "evaluate"

--    let cnt n = (1+) $ sum $ map cnt $ IR.getChildren n
--    let ids n = Set.insert (IR.getID n) $ Set.unions $ map ids $ IR.getChildren n

--    print  ("cnt", cnt ir')
--    print  ("ids", ids ir')

-- (isCallExpr)
    let nodes = collectAllPrune (const True) (const NoPrune) $ mkNodeAddress [] ir
    let paths = collectAllPrunePaths (const True) (const NoPrune) ir
--    let nodes' = collectAllPrune' isCallExpr ir

--    let cnt n = (1+) $ sum $ map cnt $ IR.getChildren n

--    print $ cnt ir

    print $ length paths
--    print $ take 100 nodes
--    mapM_ evaluate $ map force nodes
--    evaluate $ force nodes

--(flip mkNodeAddress ir)
-- mkNodeAddresses' ir
--    evaluate $ force $ map getPathReversed $ mkNodeAddresses ir paths


    -- print ("is sorted", sort nodes' == nodes')
    -- print (" elems", take 10 (sort nodes'))
    -- print ("selems", take 10       nodes')

    -- print ("is same", sort nodes == sort nodes')
    -- print ("len", length nodes, length nodes')

    return ()

isCallExpr :: IR.Tree -> Bool
isCallExpr a | IR.getNodeType a == IR.CallExpr = True
isCallExpr _ = False
