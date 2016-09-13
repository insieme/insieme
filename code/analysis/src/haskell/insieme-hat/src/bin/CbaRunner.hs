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

import Control.Monad.State.Strict (State, evalState, forM_, get, put)
import Data.ByteString (getContents)
import Data.List (isPrefixOf)
import Data.Text.Format as Fmt
import Data.Text.Lazy.Builder (fromString)

import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as CV
import qualified Insieme.Analysis.Solver as Solver

import qualified Insieme.Inspire as IR
import Insieme.Inspire.BinaryParser (parseBinaryDump)
import qualified Insieme.Inspire.NodeAddress as Addr
import Insieme.Inspire.Utils (foldTree)

import Insieme.Utils.Arithmetic (NumOrdering(NumEQ), numCompare)
import qualified Insieme.Utils.BoundSet as BSet

main :: IO ()
main = do
    dump <- Data.ByteString.getContents
    let Right ir = parseBinaryDump dump
    let findings = foldTree findAnalysis ir
    let results = evalState (sequence $ analysis <$> findings) Solver.initState
    forM_ results line
    return ()
  where
    cba  = Fmt.right 40 ' ' . getCbaExpect
    addr = Fmt.right 40 ' ' . Addr.prettyShow . getAddr
    res  = fromString . show . getResult
    line x = Fmt.print "{} {}: {}\n" [cba x, addr x, res x]

data AnalysisRun = AnalysisRun
  { getAddr      :: Addr.NodeAddress
  , getCbaExpect :: String
  , getResult    :: AnalysisResult } deriving (Eq, Show)

data AnalysisResult = Ok | Inaccurate | Fail | Pending
  deriving (Eq, Show, Read)

isPending :: AnalysisRun -> Bool
isPending = (==Pending) . getResult

findAnalysis :: Addr.NodeAddress -> [AnalysisRun] -> [AnalysisRun]
findAnalysis addr acc =
    case Addr.getNodePair addr of
        IR.NT IR.CallExpr (_:IR.NT IR.Literal [_, IR.NT (IR.StringValue s) _]:_) | "IMP_cba_expect" `isPrefixOf` s
            -> AnalysisRun addr s Pending : acc
        _   -> acc

aliasAnalysis :: AnalysisRun -> State Solver.SolverState Alias.Results
aliasAnalysis a = do
    state <- get
    let (res, state') = Alias.checkAlias state (Addr.goDown 1 $ Addr.goDown 2 $ getAddr a) (Addr.goDown 1 $ Addr.goDown 3 $ getAddr a)
    put state'
    return res

boolAnalysis :: AnalysisRun -> State Solver.SolverState AnBoolean.Result
boolAnalysis a = do
    state <- get
    let (res, state') = Solver.resolve state $ AnBoolean.booleanValue $ Addr.goDown 2 $ getAddr a
    put state'
    return $ CV.toValue res

arithAnalysis :: AnalysisRun -> State Solver.SolverState (Arith.SymbolicFormulaSet BSet.Bound10)
arithAnalysis a = do
    state <- get
    let (res, state') = Solver.resolve state $ Arith.arithmeticValue $ Addr.goDown 2 $ getAddr a
    put state'
    return $ CV.toValue res

arithAnalysis2 :: AnalysisRun -> State Solver.SolverState (Arith.SymbolicFormulaSet BSet.Bound10, Arith.SymbolicFormulaSet BSet.Bound10)
arithAnalysis2 a = do
    state <- get
    let (lhs, state')  = Solver.resolve state  $ Arith.arithmeticValue $ Addr.goDown 2 $ getAddr a
    let (rhs, state'') = Solver.resolve state' $ Arith.arithmeticValue $ Addr.goDown 3 $ getAddr a
    put state''
    return (CV.toValue lhs, CV.toValue rhs)

analysis :: AnalysisRun -> State Solver.SolverState AnalysisRun
analysis a | not (isPending a) = return a
analysis a =
    case getCbaExpect a of

        -- alias
        "IMP_cba_expect_ref_are_alias" -> do
            res <- aliasAnalysis a
            return a{getResult = if res == Alias.AreAlias then Ok else Fail}

        "IMP_cba_expect_ref_may_alias" -> do
            res <- aliasAnalysis a
            return a{getResult = if res == Alias.MayAlias then Ok else Fail}

        "IMP_cba_expect_ref_not_alias" -> do
            res <- aliasAnalysis a
            return a{getResult = if res == Alias.NotAlias then Ok else Fail}

        -- boolean
        "IMP_cba_expect_true" -> do
            res <- boolAnalysis a
            return a{getResult = if res == AnBoolean.AlwaysTrue then Ok else Fail}

        "IMP_cba_expect_false" -> do
            res <- boolAnalysis a
            return a{getResult = if res == AnBoolean.AlwaysFalse then Ok else Fail}

        "IMP_cba_expect_may_be_true" -> do
            res <- boolAnalysis a
            return a{getResult = if res `elem` [AnBoolean.AlwaysTrue, AnBoolean.Both] then Ok else Fail}

        "IMP_cba_expect_may_be_false" -> do
            res <- boolAnalysis a
            return a{getResult = if res `elem` [AnBoolean.AlwaysFalse, AnBoolean.Both] then Ok else Fail}

        "IMP_cba_expect_undefined_int" -> do
            res <- arithAnalysis a
            return $ case () of _
                                 | BSet.isUniverse res -> a{getResult = Ok}
                                 | BSet.size res > 0   -> a{getResult = Inaccurate}
                                 | otherwise           -> a{getResult = Fail}
        "IMP_cba_expect_eq_int" -> do
            (lhs, rhs) <- arithAnalysis2 a
            return a{getResult = if all (==NumEQ) $ BSet.toList $ BSet.lift2 numCompare lhs rhs then Ok else Fail}

        "IMP_cba_expect_ne_int" -> do
            (lhs, rhs) <- arithAnalysis2 a
            return a{getResult = if notElem NumEQ $ BSet.toList $ BSet.lift2 numCompare lhs rhs then Ok else Fail}


        "IMP_cba_expect_may_eq_int" -> do
            (lhs, rhs) <- arithAnalysis2 a
            return $ case () of _
                                 | BSet.isUniverse lhs || BSet.isUniverse rhs -> a{getResult = Ok}
                                 | BSet.size (BSet.intersection lhs rhs) > 0  -> a{getResult = Ok}
                                 | otherwise                                  -> a{getResult = Fail}

        _ -> return a -- error "Unsupported Analysis"
