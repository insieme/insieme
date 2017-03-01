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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Insieme.Analysis.Entities.FieldIndex (SimpleFieldIndex)
import Insieme.Inspire.Visit (foldTreePrune)
import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Query as Q
import qualified Insieme.Utils.BoundSet as BSet

main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right ir = BinPar.parseBinaryDump dump

    let targets = foldTreePrune findTargets (Q.isType) ir

    let res = evalState (sequence $ analysis <$> targets) Solver.initState

    putStr $ "Errors:  " ++ (show $ length $ filter (=='e') res) ++ "\n"
    putStr $ "Unknown: " ++ (show $ length $ filter (=='u') res) ++ "\n"
    putStr $ "OK:      " ++ (show $ length $ filter (=='o') res) ++ "\n"

findTargets :: Addr.NodeAddress -> [Addr.NodeAddress] -> [Addr.NodeAddress]
findTargets addr xs = case Addr.getNode addr of
    IR.Node IR.CallExpr _ | Q.isBuiltin (Addr.goDown 1 addr) "ref_deref" -> addr : xs
    _ -> xs

analysis :: Addr.NodeAddress -> State Solver.SolverState Char
analysis addr = do
    state <- get
    let (res, state') = Solver.resolve state (Ref.referenceValue $ Addr.goDown 1 $ Addr.goDown 2 addr)
    put state'
    let refs = ComposedValue.toValue res :: BSet.UnboundSet (Ref.Reference SimpleFieldIndex)
    return $ case () of _
                         | BSet.null refs       -> 'e'
                         | BSet.isUniverse refs -> 'u'
                         | otherwise            -> 'o'
