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

module Insieme.Analysis.Solver.Constraint
    ( Constraint(..)
    , createConstraint
    , createEqualityConstraint

    , getDependencies
    , getLimit

    , forward
    , forwardIf
    , constant

    , Event(..)
    , Violation(..)
    ) where

import Prelude hiding (print)

import qualified Data.Set as Set

import {-# SOURCE #-} Insieme.Analysis.Solver.Var
import Insieme.Analysis.Solver.Assignment
import Insieme.Analysis.Solver.AssignmentView
import Insieme.Analysis.Solver.Lattice
import Insieme.Analysis.Solver.SolverState

-- Constraints ---------------------------------------------

data Event = None       -- ^ an update had no effect
           | Increment  -- ^ an update was an incremental update
           | Reset      -- ^ an update was not incremental

data Constraint = Constraint 
    { dependingOn        :: AssignmentView -> [Var]
        -- ^ obtains list of variables depending on
    , update             :: (AssignmentView -> (Assignment, Event))
        -- ^ update the assignment, a reset is allowed
    , updateWithoutReset :: (AssignmentView -> (Assignment, Event))
        -- ^ update the assignment, a reset is not allowed
    , check              :: SolverState -> Maybe Violation
        -- ^ check whether this constraint is satisfied
    , printLimit         :: AssignmentView -> String
        -- ^ requests to print the current limit defined (debugging)
    }

data Violation = Violation 
    { violationMsg         :: String -- ^ a message describing the issue
    , violationShouldValue :: String -- ^ the value a variable should have (at least)
    , violationIsValue     :: String -- ^ the value a variable does have
    }

getDependencies :: AssignmentView -> TypedVar a -> [Var]
getDependencies a v = concat $ (go <$> (constraints . toVar) v)
    where
        go c = dependingOn c a

getLimit :: Lattice a => AssignmentView -> TypedVar a -> a
getLimit a v = join (go <$> (constraints . toVar) v)
    where
        go c = get' a' v
            where
                (a',_) = update c a


-- | A simple constraint factory, taking as arguments
--   * a function to return the dependent variables of this constraint,
--   * the current value of the constraint,
--   * and the target variable for this constraint.
createConstraint :: Lattice a => ( AssignmentView -> [Var] ) -> ( AssignmentView -> a ) -> TypedVar a -> Constraint
createConstraint dep limit trg = Constraint dep update' update' check' printLimit'
    where
        update' fa = case () of
                _ | value `less` current -> (                                a,      None)    -- nothing changed
                _                        -> (set a trg (value `merge` current), Increment)    -- an incremental change
            where
                value = limit fa                                                              -- the value from the constraint
                current = get' a trg                                                          -- the current value in the assignment
                a = stripFilter fa

        check' state = 
                if value `less` current then Nothing
                else Just $ Violation (print') (show value) (show current)
            where
                a = assignment state
                ua = UnfilteredView a
                fa = FilteredView (dep ua) a
                value = limit fa
                current = get' a trg

        print' = "f(A) => g(A) ⊑ " ++ (show trg)

        printLimit' a = print $ limit a

-- creates a constraint of the form f(A) = A[b] enforcing equality
createEqualityConstraint :: Lattice t => (AssignmentView -> [Var]) -> (AssignmentView -> t) -> TypedVar t -> Constraint
createEqualityConstraint dep limit trg = Constraint dep update forceUpdate check' printLimit'
    where
        update fa = case () of
                _ | value `less` current -> (              a,      None)    -- nothing changed
                _ | current `less` value -> (set a trg value, Increment)    -- an incremental change
                _                        -> (set a trg value,     Reset)    -- a reseting change, heading in a different direction

            where
                value = limit fa                                            -- the value from the constraint
                current = get' a trg                                        -- the current value in the assignment
                a = stripFilter fa

        forceUpdate fa = case () of
                _ | value `less` current -> (                                a,      None)    -- nothing changed
                _                        -> (set a trg (value `merge` current), Increment)    -- an incremental change
            where
                value = limit fa                                                              -- the value from the constraint
                current = get' a trg                                                          -- the current value in the assignment
                a = stripFilter fa

        check' state = case () of
                _ | value `less` current && current `less` value -> Nothing             -- perfect, it is equal!
                _ | value `less` current && inCycle              -> Nothing             -- it is not equal, but in a cycle ... acceptable
                _ | otherwise -> Just $ Violation (print') (show value) (show current)  -- not so good :(
            where
                a = assignment state
                ua = UnfilteredView a
                fa = FilteredView (dep ua) a
                value = limit fa
                current = get' a trg

                -- compute the set of all variables the constraint variable is depending on
                allDependencies = collect (dep ua) Set.empty
                    where
                        collect [] s = s
                        collect (v:vs) s = collect ((Set.toList $ Set.difference dep s) ++ vs) (Set.union dep s)
                            where
                                dep = Set.fromList $ concatMap (flip dependingOn ua) $ constraints v

                -- determine whether the constraint variable is indeed in a active cycle
                inCycle = Set.member (toVar trg) allDependencies


        print' = "f(A) => g(A) = " ++ (show trg) ++ " with " ++ (show $ length $ constraints $ toVar trg) ++ " constraints"

        printLimit' a = print $ limit a


-- creates a constraint of the form   A[a] \in A[b]
forward :: Lattice a => TypedVar a -> TypedVar a -> Constraint
forward a@(TypedVar v) b = createConstraint (\_ -> [v]) (\a' -> get a' a) b


-- creates a constraint of the form  x \sub A[a] => A[b] \in A[c]
forwardIf :: (Lattice a, Lattice b) => a -> TypedVar a -> TypedVar b -> TypedVar b -> Constraint
forwardIf a b@(TypedVar v1) c@(TypedVar v2) d = createConstraint dep upt d
    where
        dep = (\a' -> if less a $ get a' b then [v1,v2] else [v1] )
        upt = (\a' -> if less a $ get a' b then get a' c else bot )


-- creates a constraint of the form  x \in A[b]
constant :: Lattice a => a -> TypedVar a -> Constraint
constant x b = createConstraint (const []) (const x) b
