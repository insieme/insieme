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

import Control.Monad
import Data.List
import Data.Text.Format as Fmt
import Data.Text.Lazy.Builder (fromString)
import Data.Tree (Tree(Node))
import Debug.Trace
import qualified Data.ByteString as BS
import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Utils as Utils
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet



main :: IO ()
main = do
    -- read in binary dump of IR
    dump <- BS.getContents

    -- run parser
    let Right ir = BinPar.parseBinaryDump dump

    forM (Utils.foldTree findAnalysis ir) line

    return ()

  where
    cba  = Fmt.right 40 ' ' . getCbaExpect
    addr = Fmt.right 40 ' ' . Addr.prettyShow . getAddr
    res  = fromString . show . getResult
    line x = Fmt.print "{} {}: {}\n" [cba x, addr x, res x]



data AnalysisRun = AnalysisRun { getAddr      :: Addr.NodeAddress,
                                 getCbaExpect :: String,
                                 getResult    :: AnalysisResult }
  deriving (Eq, Show)

data AnalysisResult = Ok | Inaccurate | Fail
  deriving (Eq, Show, Read)



findAnalysis :: Addr.NodeAddress -> [AnalysisRun] -> [AnalysisRun]
findAnalysis addr acc =
    case Addr.getNode addr of
        Node IR.CallExpr (_:Node IR.Literal [_, Node (IR.StringValue s ) _]:_) | isPrefixOf "IMP_cba_expect" s -> AnalysisRun addr s (analysis s) : acc
        _ -> acc
  where
    resolve a = ComposedValue.toValue . fst . (Solver.resolve Solver.initState) . a

    res_boolean = resolve AnBoolean.booleanValue $ Addr.goDown 2 addr

    res_arithme   = resolve Arith.arithmeticValue $ Addr.goDown 2 addr
    res_arithme_2 = resolve Arith.arithmeticValue $ Addr.goDown 3 addr

    res_alias = Alias.checkAlias (Addr.goDown 2 addr) ( Addr.goDown 3 addr)

    analysis s = case s of

        -- alias
        "IMP_cba_expect_ref_are_alias" | res_alias == Alias.AreAlias -> Ok
        "IMP_cba_expect_ref_are_alias" -> Fail

        "IMP_cba_expect_ref_may_alias" | res_alias == Alias.MayAlias -> Ok
        "IMP_cba_expect_ref_may_alias" -> Fail

        "IMP_cba_expect_ref_not_alias" | res_alias == Alias.NotAlias -> Ok
        "IMP_cba_expect_ref_not_alias" -> Fail

        -- boolean
        "IMP_cba_expect_true" | res_boolean == AnBoolean.AlwaysTrue -> Ok
        "IMP_cba_expect_true" -> Fail

        "IMP_cba_expect_false" | res_boolean == AnBoolean.AlwaysFalse -> Ok
        "IMP_cba_expect_false" -> Fail

        "IMP_cba_expect_may_be_true" | res_boolean `elem` [AnBoolean.AlwaysTrue, AnBoolean.Both] -> Ok
        "IMP_cba_expect_may_be_true" -> Fail

        "IMP_cba_expect_may_be_false" | res_boolean `elem` [AnBoolean.AlwaysFalse, AnBoolean.Both] -> Ok
        "IMP_cba_expect_may_be_false" -> Fail

        -- arithmetic
        "IMP_cba_expect_undefined_int" | BSet.isUniverse res_arithme -> Ok
        "IMP_cba_expect_undefined_int" | BSet.size res_arithme > 0   -> Inaccurate
        "IMP_cba_expect_undefined_int" -> Fail

        "IMP_cba_expect_eq_int" | all (==Ar.NumEQ) $ BSet.toList $ BSet.lift2 Ar.numCompare res_arithme res_arithme_2 -> Ok
        "IMP_cba_expect_eq_int" -> Fail

        "IMP_cba_expect_ne_int" | all (/=Ar.NumEQ) $ BSet.toList $ BSet.lift2 Ar.numCompare res_arithme res_arithme_2 -> Ok
        "IMP_cba_expect_ne_int" -> Fail

        "IMP_cba_expect_may_eq_int" | oneIsUniverse || BSet.size (BSet.intersection res_arithme res_arithme_2) > 0 -> Ok
        "IMP_cba_expect_may_eq_int" -> Fail

        _ -> error "Unsupported Analysis"

    oneIsUniverse = BSet.isUniverse res_arithme || BSet.isUniverse res_arithme_2
