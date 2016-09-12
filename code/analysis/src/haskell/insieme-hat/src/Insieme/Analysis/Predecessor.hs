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

{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Predecessor (
    PredecessorList,
    predecessor
) where

import Data.Maybe
import Data.Typeable
import Insieme.Analysis.Boolean
import Insieme.Analysis.CallSite
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.ExitPoint
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Utils
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Inspire as IR
import qualified Insieme.Utils.UnboundSet as USet

--
-- * Predecessor Lattice
--

type PredecessorList = [ProgramPoint]

instance Solver.Lattice PredecessorList where
    join [] = []
    join xs = foldr1 (++) xs

--
-- * Predecessor Analysis
--

data PredecessorAnalysis = PredecessorAnalysis
    deriving (Typeable)

predecessorAnalysis :: Solver.AnalysisIdentifier
predecessorAnalysis = Solver.mkAnalysisIdentifier PredecessorAnalysis "pred_of"


--
-- * Predecessor Variable Generator
--

-- | Given a program point (Pre, Post, or Internal), give the
-- predecessors for it as a constraint.
predecessor :: ProgramPoint -> Solver.TypedVar PredecessorList

-- Predecessor rules for the program root node
predecessor p@(ProgramPoint a Pre) | isRoot a = var
    where
        var = Solver.mkVariable (idGen p) [] []

-- Predecessor rules for pre program points
predecessor p@(ProgramPoint a Pre) = case getNodePair parent of

    IR.NT IR.CallExpr children -> single $
            if i == length children-1
            -- start with last argument
            then ProgramPoint parent Pre
            -- eval arguments in reverse order
            else ProgramPoint (goDown (i+1) parent) Post

    IR.NT IR.Lambda   _ -> call_sites

    IR.NT IR.BindExpr _ -> call_sites

    IR.NT IR.TupleExpr _ -> single $ ProgramPoint parent Pre

    IR.NT IR.InitExpr _
      | i == 1     -> single $ ProgramPoint (goDown 2 parent) Post
      | otherwise -> single $ ProgramPoint parent Pre

    IR.NT IR.CastExpr _ -> single $ ProgramPoint parent Pre

    IR.NT IR.Expressions _
      | i == 0     -> single $ ProgramPoint parent Pre
      | otherwise -> single $ ProgramPoint (goDown (i-1) parent) Post

    IR.NT IR.Declaration _ -> single $ ProgramPoint parent Pre

    IR.NT IR.DeclarationStmt _ -> single $ ProgramPoint parent Pre

    IR.NT IR.CompoundStmt _ -> single $
      -- if it is the first statement
      if i == 0
      -- then go to the pre-state of the compound statement
      then ProgramPoint parent Pre
      -- else to the post state of the previous statement
      else ProgramPoint (goDown (i-1) parent) Post

    IR.NT IR.IfStmt _
      | i == 0     -> single $ ProgramPoint parent Pre
      -- TODO: make dependent on result of conditional expression
      | otherwise -> single $ ProgramPoint (goDown 0 parent) Post

    IR.NT IR.ForStmt _
      -- pred of declarations is loop itself
      | i == 0 -> single $ ProgramPoint parent Pre
      -- pred of end value is declaration block
      | i == 1 -> single $ ProgramPoint (goDown 0 parent) Post
      -- pred of step value is end value
      | i == 2 -> single $ ProgramPoint (goDown 1 parent) Post
      -- pred of body is 1. step value, 2. previous body, or 3. continue stmt
      | i == 3 -> multiple $
                [ ProgramPoint (goDown 2 parent) Post    -- step
                , ProgramPoint (goDown 3 parent) Post]   -- body (n-1)
                ++ postContinueStmt a                    -- all continues

    IR.NT IR.WhileStmt _
      -- cond of while is evaluated 1. after entering while, 2. after
      -- regular loop, and 3. after a continue
      | i == 0 -> multiple $
                [ ProgramPoint parent Pre                -- entering
                , ProgramPoint (goDown 1 parent) Post]   -- regular loop
                ++ postContinueStmt a                    -- all continues
      -- body of a while is evaluated only after the condition
      | i == 1 -> single $ ProgramPoint (goDown 0 parent) Post

    IR.NT IR.SwitchStmt _
      -- pred of the switch expression is the switch stmt itself
      | i == 0     -> single $ ProgramPoint parent Pre
      -- pred of a branch always is the switch expression
      | otherwise -> single $ ProgramPoint (goRel [-1, 0] a) Post

    -- after processing a lone BreakStmt we end up here; go to switch expr
    IR.NT IR.SwitchCase _ -> single . flip ProgramPoint Post $ goRel [-3, 0] a

    IR.NT IR.ReturnStmt _ -> single $ ProgramPoint parent Pre

    _ -> unhandled "Pre" p (getNodeType parent)

  where
    parent = fromJust $ getParent a
    i = getIndex a
    single = single' p
    multiple = multiple' p
    call_sites = Solver.mkVariable (idGen p) [con] []
    con = Solver.createConstraint dep val call_sites
    dep a = [Solver.toVar callSitesVar]
    val a = foldr go [] $ Solver.get a callSitesVar
    go (CallSite call) list = ProgramPoint (goDown 1 call) Post : list
    callSitesVar = callSites parent


-- Predecessor rules for internal program points.
predecessor  p@(ProgramPoint addr Internal) = case getNodePair addr of

    -- link to exit points of potential target functions
    IR.NT IR.CallExpr _ -> var
      where
        extract = ComposedValue.toValue
        var = Solver.mkVariable (idGen p) [con] []
        con = Solver.createConstraint dep val var
        dep a = Solver.toVar callableVar : map Solver.toVar (exitPointVars a)
        val a = [litPredecessor | callsLiteral] ++ nonLiteralExit
          where
            nonLiteralExit = foldr go [] (exitPointVars a)
            go e l = foldr (\(ExitPoint r) l -> ProgramPoint r Post : l)
                     l (Solver.get a e)
            callsLiteral = any isLiteral (callableVal a)
              where
                isLiteral (Literal _) = True
                isLiteral _ = False
            litPredecessor = ProgramPoint (goDown 1 addr) Post
        callableVar = callableValue (goDown 1 addr)
        callableVal a = USet.toSet $
                if USet.isUniverse callables
                then collectAllCallables addr
                else callables
            where
              callables = extract $ Solver.get a callableVar
        exitPointVars a =
          foldr (\t l -> exitPoints (toAddress t) : l) [] (callableVal a)

    _ -> unhandled "Internal" p (getNodeType $ fromJust $ getParent addr)

-- Predecessor rules for post program points.
predecessor p@(ProgramPoint a Post) = case getNodePair a of

    -- basic expressions are directly switching from Pre to Post
    IR.NT IR.Variable        _ -> pre
    IR.NT IR.Literal         _ -> pre
    IR.NT IR.LambdaExpr      _ -> pre
    IR.NT IR.LambdaReference _ -> pre
    IR.NT IR.BindExpr        _ -> pre
    IR.NT IR.JobExpr         _ -> pre

    -- call expressions are switching from Internal to Post
    IR.NT IR.CallExpr _ -> single $ ProgramPoint a Internal

    -- for tuple expressions, the predecessor is the end of the epxressions
    IR.NT IR.TupleExpr _ -> single $ ProgramPoint (goDown 1 a) Post

    -- for initialization expressions, we finish with the first sub-expression
    IR.NT IR.InitExpr _ -> single $ ProgramPoint (goDown 1 a) Post

    -- cast expressions just process the nested node
    IR.NT IR.CastExpr _ -> single $ ProgramPoint (goDown 1 a) Post

    -- declarationns are done once the init expression is done
    IR.NT IR.Declaration _ -> single $ ProgramPoint (goDown 1 a) Post

    -- handle lists of expressions
    IR.NT IR.Expressions  [] -> single $ ProgramPoint a Pre
    IR.NT IR.Expressions sub ->
      single $ ProgramPoint (goDown (length sub-1) a) Post

    -- compound statements
    IR.NT IR.CompoundStmt [] -> pre
    IR.NT IR.CompoundStmt stmts ->
      single $ ProgramPoint (goDown (length stmts-1) a) Post

    -- declaration statement
    IR.NT IR.DeclarationStmt _ -> single $ ProgramPoint (goDown 0 a) Post

    -- conditional statement
    IR.NT IR.IfStmt _ -> var
      where
        var = Solver.mkVariable (idGen p) [con] []
        con = Solver.createConstraint dep val var
        dep a = [Solver.toVar conditionValueVar]
        val a = case ComposedValue.toValue (Solver.get a conditionValueVar) of
                  Neither     -> []
                  AlwaysTrue  -> [thenBranch]
                  AlwaysFalse -> [elseBranch]
                  Both        -> [thenBranch,elseBranch]
        conditionValueVar = booleanValue $ goDown 0 a
        thenBranch = ProgramPoint (goDown 1 a) Post
        elseBranch = ProgramPoint (goDown 2 a) Post

    -- for loop statement
    IR.NT IR.ForStmt _ -> multiple
      [ ProgramPoint (goDown 2 a) Post     -- loop never entered
      , ProgramPoint (goDown 3 a) Post ]   -- body when loop was run

    -- while stmts evaluate as a last step either 1. their condition,
    -- or 2. a BreakStmt
    IR.NT IR.WhileStmt _ -> multiple $ map (`ProgramPoint` Post) $
      goDown 0 a:                          -- condition
      collectAddr IR.BreakStmt prune a     -- all subordinate 'BreakStmt's
      where
        prune = [(== IR.SwitchStmt), (== IR.WhileStmt), isType]

    -- moving backwards after continue stmt we jump to its beginning
    IR.NT IR.ContinueStmt _ -> pre

    -- switch stmts have executed either 1. the default branch, 2. one
    -- of the breaks, or 3. one of the cases without a break
    IR.NT IR.SwitchStmt _ -> multiple $ map (`ProgramPoint` Post) $
      goDown 2 a :                         -- default branch
      collectAddr IR.BreakStmt prune a ++  -- break statements
      [goRel [1, i, 1] a | i <- enumFromTo 0 (numChildren (goDown 1 a)-1)] -- case
      where
        prune = [(== IR.SwitchStmt), (== IR.WhileStmt), isType]

    -- moving backwards after break stmt we jump to its beginning
    IR.NT IR.BreakStmt _ -> pre

    -- return statement
    IR.NT IR.ReturnStmt _ -> single $ ProgramPoint (goDown 0 a) Post

    _ -> unhandled "Post" p (getNodeType a)

  where
    multiple = multiple' p
    single = single' p
    pre = single $ ProgramPoint a Pre

-- | Create a dependence to multiple program points.
multiple' :: ProgramPoint -> [ProgramPoint] -> Solver.TypedVar PredecessorList
multiple' p = Solver.mkVariable (idGen p) []

-- | Create a dependence to a program point.
single' :: ProgramPoint -> ProgramPoint -> Solver.TypedVar PredecessorList
single' p x = multiple' p [x]

-- | 'Post' 'ProgramPoint's for all 'ContinueStmt' nodes directly
-- below the given address.
postContinueStmt :: NodeAddress -> [ProgramPoint]
postContinueStmt a = map (`ProgramPoint` Post)
                     (collectAddr IR.ContinueStmt prune a)
  where prune = [(== IR.WhileStmt), (== IR.ForStmt), (== IR.LambdaExpr), isType]

-- | Variable ID generator
idGen :: ProgramPoint -> Solver.Identifier
idGen (ProgramPoint a p) = Solver.mkIdentifier predecessorAnalysis a (show p)

-- | Unhandled cases should print an error message for easier debugging.
unhandled :: String -> ProgramPoint -> IR.NodeType
          -> Solver.TypedVar PredecessorList
unhandled pos p parent = error . unwords $
  ["Unhandled", pos, "Program Point:", show p, "for parent", show parent]
