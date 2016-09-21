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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Insieme.Analysis.Entities.FieldIndex (SimpleFieldIndex)
import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Utils as Utils
import qualified Insieme.Utils.UnboundSet as USet



main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right ir = BinPar.parseBinaryDump dump

    let targets = Utils.foldTreePrune findTargets (Utils.isType . Addr.getNodeType) ir

    let res = evalState (sequence $ analysis <$> targets) Solver.initState

    putStr $ "Errors:  " ++ (show $ length $ filter (=='e') res) ++ "\n"
    putStr $ "Unknown: " ++ (show $ length $ filter (=='u') res) ++ "\n"
    putStr $ "OK:      " ++ (show $ length $ filter (=='o') res) ++ "\n"

findTargets :: Addr.NodeAddress -> [Addr.NodeAddress] -> [Addr.NodeAddress]
findTargets addr xs = case Addr.getNodePair addr of
    IR.NT IR.CallExpr _ | Addr.isBuiltinByName (Addr.goDown 1 addr) "ref_deref" -> addr : xs
    _ -> xs

analysis :: Addr.NodeAddress -> State Solver.SolverState Char
analysis addr = do
    state <- get
    let (res, state') = Solver.resolve state (Ref.referenceValue $ Addr.goDown 1 $ Addr.goDown 2 addr)
    put state'
    let refs = ComposedValue.toValue res :: USet.UnboundSet (Ref.Reference SimpleFieldIndex)
    return $ case () of _
                         | USet.null refs       -> 'e'
                         | USet.isUniverse refs -> 'u'
                         | otherwise            -> 'o'
