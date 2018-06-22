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
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Predecessor (
    PredecessorList(..),
    predecessor
) where

import Data.List
import Data.Maybe
import Control.DeepSeq (NFData)
import Data.Typeable
import GHC.Generics (Generic)

import qualified Data.Set as Set

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Query as Q
import qualified Insieme.Inspire as I
import qualified Insieme.Utils.BoundSet as BSet

import Insieme.Analysis.Boolean
import Insieme.Analysis.CallSite
import Insieme.Analysis.Callable
import Insieme.Analysis.Entities.ProgramPoint
import Insieme.Analysis.ExitPoint
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Utils.CppSemantic as Sema
import qualified Insieme.Solver as Solver

--
-- * Predecessor Lattice
--

newtype PredecessorList = PredecessorList { unPL :: Set.Set ProgramPoint }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Solver.Lattice PredecessorList where
    bot = PredecessorList Set.empty
    (PredecessorList x) `merge` (PredecessorList y) = PredecessorList $ Set.union x y

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
predecessor p@(ProgramPoint a Pre) | I.isRoot a = var
    where
        var = Solver.mkVariable (idGen p) [] Solver.bot

-- Predecessor rules for pre program points
predecessor p@(ProgramPoint a Pre) = case Q.getNodeType parent of

    I.CallExpr -> single $
            if i == (I.numChildren parent) - 1
            -- start with last argument
            then ProgramPoint parent Pre
            -- eval arguments in reverse order
            else ProgramPoint (I.goDown (i+1) parent) Post

    I.Lambda -> case () of
          _ | isImplicitCtor ->
                single $ ProgramPoint (I.goDown 1 $ Sema.getEnclosingDeclaration parent) Post
            | isImplicitDtor -> case () of
                _ | dtorIndex == 0 -> single $ ProgramPoint (I.goDown ((I.numChildren scope) -1) scope) Post
                  | otherwise      -> single $ ProgramPoint (dtors !! (dtorIndex-1)) Post
            | otherwise      -> call_sites
        where
            isImplicitCtor = Sema.isImplicitConstructor parent
            isImplicitDtor = Sema.isImplicitDestructor  parent

            scope = Sema.getEnclosingScope parent

            dtors = Sema.getImplicitDestructorBodies scope
            dtorIndex = fromJust $ elemIndex a dtors


    I.BindExpr -> call_sites

    I.TupleExpr -> single $ ProgramPoint parent Pre

    I.InitExpr
      | i == 1    -> single $ ProgramPoint (I.goDown 2 parent) Post
      | otherwise -> single $ ProgramPoint parent Pre

    I.CastExpr -> single $ ProgramPoint parent Pre

    I.Expressions
      | i == 0    -> single $ ProgramPoint parent Pre
      | otherwise -> single $ ProgramPoint (I.goDown (i-1) parent) Post

    I.Declaration -> single $ ProgramPoint parent Pre

    I.Declarations
      | i == 0    -> single $ ProgramPoint parent Pre
      | otherwise -> single $ ProgramPoint (I.goDown (i-1) parent) Post

    I.DeclarationStmt -> single $ ProgramPoint parent Pre

    I.CompoundStmt ->
        -- if it is the first statement
        if i == 0
        -- then go to the pre-state of the compound statement
        then single $ ProgramPoint parent Pre
        -- else to the post state of the previous statement
        else predecessorPoint

      where

        predecessor = I.goDown (i-1) parent
        predecessorPoint = case Q.getNodeType predecessor of
            I.ReturnStmt   -> none
            I.BreakStmt    -> none
            I.ContinueStmt -> none
            _              -> single $ ProgramPoint predecessor Post

    I.IfStmt
      | i == 0    -> single $ ProgramPoint parent Pre
      -- TODO: make dependent on result of conditional expression
      | otherwise -> single $ ProgramPoint (I.goDown 0 parent) Post

    I.ForStmt
      -- pred of declarations is loop itself
      | i == 0 -> single $ ProgramPoint parent Pre
      -- pred of end value is declaration block
      | i == 1 -> single $ ProgramPoint (I.goDown 0 parent) Post
      -- pred of step value is end value
      | i == 2 -> single $ ProgramPoint (I.goDown 1 parent) Post
      -- pred of body is 1. step value, 2. previous body, or 3. continue stmt
      | i == 3 -> multiple $ PredecessorList $ Set.fromList $
                [ ProgramPoint (I.goDown 2 parent) Post    -- step
                , ProgramPoint (I.goDown 3 parent) Post]   -- body (n-1)
                ++ postContinueStmt a                    -- all continues

    I.WhileStmt
      -- cond of while is evaluated 1. after entering while, 2. after
      -- regular loop, and 3. after a continue
      | i == 0 -> multiple $ PredecessorList $ Set.fromList $
                [ ProgramPoint parent Pre                -- entering
                , ProgramPoint (I.goDown 1 parent) Post]   -- regular loop
                ++ postContinueStmt a                    -- all continues
      -- body of a while is evaluated only after the condition
      | i == 1 -> single $ ProgramPoint (I.goDown 0 parent) Post

    I.SwitchStmt
      -- pred of the switch expression is the switch stmt itself
      | i == 0     -> single $ ProgramPoint parent Pre
      -- pred of a branch always is the switch expression
      | otherwise -> single $ ProgramPoint (I.goRel [-1, 0] a) Post

    -- after processing a lone BreakStmt we end up here; go to switch expr
    I.SwitchCase -> single . flip ProgramPoint Post $ I.goRel [-3, 0] a

    I.ReturnStmt -> single $ ProgramPoint parent Pre

    -- if the parent is types, we are in an implicit intercepted constructor
    I.Types -> single $ ProgramPoint (I.goDown 1 decl) Post  -- end if init expression
      where
        decl = Sema.getEnclosingDeclaration parent

    _ -> unhandled "Pre" p (Q.getNodeType parent)

  where
    parent = fromJust $ I.getParent a

    i = I.getIndex a

    none = none' p
    single = single' p
    multiple = multiple' p

    call_sites = Solver.mkVariable (idGen p) [con] Solver.bot
    con = Solver.createConstraint dep val call_sites

    dep _ = [Solver.toVar callSitesVar]
    val a = PredecessorList $ Set.fromList $ foldr go [] $ Solver.get a callSitesVar
      where
        go (CallSite call) list = ProgramPoint (I.goDown 1 call) Post : list

    callSitesVar = callSites parent


-- Predecessor rules for internal program points.
predecessor  p@(ProgramPoint addr Internal) = case Q.getNodeType addr of

    -- link to exit points of potential target functions
    I.CallExpr -> var
      where
        extract = ComposedValue.toValue
        var = Solver.mkVariable (idGen p) [con] Solver.bot
        con = Solver.createConstraint dep val var
        dep a = Solver.toVar callableVar : map Solver.toVar (exitPointVars a)
        val a = PredecessorList $ Set.fromList $ [litPredecessor | callsLiteral] ++ nonLiteralExit
          where
            nonLiteralExit = foldr go [] (exitPointVars a)
            go e l = foldr (\(ExitPoint r) l -> ProgramPoint r Post : l)
                     l (Solver.get a e)
            callsLiteral = any isLiteral (callableVal a)
              where
                isLiteral (Literal _) = True
                isLiteral _ = False
            litPredecessor = ProgramPoint (I.goDown 1 addr) Post
        callableVar = callableValue (I.goDown 1 addr)
        callableVal a =
                if BSet.isUniverse callables
                then collectAllCallables addr
                else callables
            where
              callables = extract $ Solver.get a callableVar
        exitPointVars a =
          foldr (\t l -> exitPoints (toAddress t) : l) [] (callableVal a)

    -- declarations with implicit constructor are done once the init expression is done
    I.Declaration | Sema.callsImplicitConstructor addr -> case Sema.getImplicitConstructor addr of
        Just ctor -> case Q.getNodeType ctor of
            -- TODO: support multiple exit points for ctor
            I.LambdaExpr -> single' p $ ProgramPoint (I.goDown 2 $ fromJust $ Q.getLambda ctor) Post
            _ -> single' p $ ProgramPoint ctor Post
        Nothing   -> error "Implicit constructor not found?!"

    -- declarations without implicit constructor are done once the init expression is done
    I.Declaration -> single' p $ ProgramPoint (I.goDown 1 addr) Post

    _ -> unhandled "Internal" p (Q.getNodeType $ fromJust $ I.getParent addr)

-- Predecessor rules for post program points.
predecessor p@(ProgramPoint a Post) = case Q.getNodeType a of

    -- basic expressions are directly switching from Pre to Post
    I.Variable        -> pre
    I.Literal         -> pre
    I.LambdaExpr      -> pre
    I.LambdaReference -> pre
    I.BindExpr        -> pre
    I.JobExpr         -> pre

    -- call expressions are switching from Internal to Post
    I.CallExpr -> single $ ProgramPoint a Internal

    -- for tuple expressions, the predecessor is the end of the epxressions
    I.TupleExpr -> single $ ProgramPoint (I.goDown 1 a) Post

    -- for initialization expressions, we finish with the first sub-expression
    I.InitExpr -> single $ ProgramPoint (I.goDown 1 a) Post

    -- cast expressions just process the nested node
    I.CastExpr -> single $ ProgramPoint (I.goDown 1 a) Post

    -- declarations without implicit constructor are done once the init expression is done
    I.Declaration -> single $ ProgramPoint a Internal

    -- handle lists of declarations
    I.Declarations | I.numChildren a == 0 -> single $ ProgramPoint a Pre
                   | otherwise            -> single $ ProgramPoint (I.goDown ((I.numChildren a) - 1) a) Post

    -- handle lists of expressions
    I.Expressions | I.numChildren a == 0 -> single $ ProgramPoint a Pre
                  | otherwise            -> single $ ProgramPoint (I.goDown ((I.numChildren a) - 1) a) Post

    -- compound statements
    I.CompoundStmt | I.numChildren a == 0 -> pre
                   | otherwise            -> case () of
                       _ | null dtors -> single $ ProgramPoint (I.goDown ((I.numChildren a) -1) a) Post
                         | otherwise  -> single $ ProgramPoint (last dtors) Post
      where
        dtors = Sema.getImplicitDestructorBodies a


    -- declaration statement
    I.DeclarationStmt -> single $ ProgramPoint (I.goDown 0 a) Post

    -- conditional statement
    I.IfStmt -> var
      where
        var = Solver.mkVariable (idGen p) [con] Solver.bot
        con = Solver.createConstraint dep val var
        dep _ = [Solver.toVar conditionValueVar]
        val a = PredecessorList $ Set.fromList $ case ComposedValue.toValue (Solver.get a conditionValueVar) of
                  Neither     -> []
                  AlwaysTrue  -> [thenBranch]
                  AlwaysFalse -> [elseBranch]
                  Both        -> [thenBranch,elseBranch]
        conditionValueVar = booleanValue $ I.goDown 0 a
        thenBranch = ProgramPoint (I.goDown 1 a) Post
        elseBranch = ProgramPoint (I.goDown 2 a) Post

    -- for loop statement
    I.ForStmt -> multiple $ PredecessorList $ Set.fromList $
      [ ProgramPoint (I.goDown 2 a) Post     -- loop never entered
      , ProgramPoint (I.goDown 3 a) Post ]   -- body when loop was run

    -- while stmts evaluate as a last step either 1. their condition,
    -- or 2. a BreakStmt
    I.WhileStmt -> multiple $ PredecessorList $ Set.fromList $ map (`ProgramPoint` Post) $
      I.goDown 0 a:                          -- condition
      I.collectAddr I.BreakStmt prune a     -- all subordinate 'BreakStmt's
      where
        prune = [(== I.SwitchStmt), (== I.WhileStmt), isType]
        isType = (==I.Type) . I.toNodeKind

    -- moving backwards after continue stmt we jump to its beginning
    I.ContinueStmt -> pre

    -- switch stmts have executed either 1. the default branch, 2. one
    -- of the breaks, or 3. one of the cases without a break
    I.SwitchStmt -> multiple $ PredecessorList $ Set.fromList $ map (`ProgramPoint` Post) $
      I.goDown 2 a :                         -- default branch
      I.collectAddr I.BreakStmt prune a ++  -- break statements
      [I.goRel [1, i, 1] a | i <- enumFromTo 0 (I.numChildren (I.goDown 1 a)-1)] -- case
      where
        prune = [(== I.SwitchStmt), (== I.WhileStmt), isType]
        isType = (==I.Type) . I.toNodeKind

    -- moving backwards after break stmt we jump to its beginning
    I.BreakStmt -> pre

    -- return statement
    I.ReturnStmt -> single $ ProgramPoint (I.goDown 0 a) Post

    -- if the program point is pointing to a type, it is an implicit constructor call
    I.GenericType  -> single $ ProgramPoint a Pre
    I.TypeVariable -> single $ ProgramPoint a Pre

    _ -> unhandled "Post" p (Q.getNodeType a)

  where
    multiple = multiple' p
    single = single' p
    pre = single $ ProgramPoint a Pre

-- | Create a dependence to multiple program points.
multiple' :: ProgramPoint -> PredecessorList -> Solver.TypedVar PredecessorList
multiple' p = Solver.mkVariable (idGen p) []

-- | Create a dependence to a program point.
single' :: ProgramPoint -> ProgramPoint -> Solver.TypedVar PredecessorList
single' p x = multiple' p $ PredecessorList $ Set.singleton x

-- | Create a dependence to no program point
none' :: ProgramPoint -> Solver.TypedVar PredecessorList
none' p = Solver.mkVariable (idGen p) [] Solver.bot

-- | 'Post' 'ProgramPoint's for all 'ContinueStmt' nodes directly
-- below the given address.
postContinueStmt :: NodeAddress -> [ProgramPoint]
postContinueStmt a = map (`ProgramPoint` Post)
                     (I.collectAddr I.ContinueStmt prune a)
  where
    prune = [(== I.WhileStmt), (== I.ForStmt), (== I.LambdaExpr), isType, isExpr]
    isType = (==I.Type) . I.toNodeKind
    isExpr = (==I.Expression) . I.toNodeKind

-- | Variable ID generator
idGen :: ProgramPoint -> Solver.Identifier
idGen pp = Solver.mkIdentifierFromProgramPoint predecessorAnalysis pp

-- | Unhandled cases should print an error message for easier debugging.
unhandled :: String -> ProgramPoint -> I.NodeType
          -> Solver.TypedVar PredecessorList
unhandled pos p parent = error . unwords $
  ["Unhandled", pos, "Program Point:", show p, "for parent", show parent]
