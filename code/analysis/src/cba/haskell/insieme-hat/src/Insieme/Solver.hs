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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- {-# Strict -} causes +15% mem and +10% runtime

module Insieme.Solver (

    -- lattices
    Lattice(..),
    ExtLattice(..),

    -- analysis identifiers
    AnalysisIdentifier,
    mkAnalysisIdentifier,

    -- identifiers
    Identifier,
    mkIdentifierFromExpression,
    mkIdentifierFromProgramPoint,
    mkIdentifierFromMemoryStatePoint,
    mkIdentifierFromString,

    -- variables
    Var,
    TypedVar,
    mkVariable,
    toVar,
    getDependencies,
    getLimit,

    -- assignment views
    AssignmentView,
    get,

    -- solver state
    SolverState,
    initState,
    assignment,
    numSteps,

    -- solver
    resolve,
    resolveS,
    resolveAll,
    solve,

    -- constraints
    createConstraint,
    createEqualityConstraint,
    forward,
    forwardIf,
    constant,

    -- debugging
    dumpSolverState,
    showSolverStatistics,

    -- metadata
    module Insieme.Solver.Metadata

) where

import Debug.Trace

import Prelude hiding (lookup,print)
import Control.Arrow
import Control.Exception
import Control.Monad (void,when)
import Control.Parallel.Strategies
import Data.List hiding (insert,lookup)
import Data.Maybe
import Data.Tuple
import System.CPUTime
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Debug.Trace

import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

import           Data.AbstractMap.Strict (Map)
import qualified Data.AbstractMap.Strict as Map
import           Data.AbstractLut (Lut)
import qualified Data.AbstractLut as Lut

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
--import           Data.Judy (JudyL)
--import qualified Data.Judy as Judy
import           Data.AbstractSet (Set)
import qualified Data.AbstractSet as Set

import Insieme.Inspire (NodePath)
import qualified Insieme.Inspire as I

import Insieme.Analysis.Entities.Memory
import Insieme.Analysis.Entities.ProgramPoint

import Insieme.Solver.Assignment
import Insieme.Solver.AssignmentView
import Insieme.Solver.Constraint
import Insieme.Solver.DebugFlags
import Insieme.Solver.Identifier
import Insieme.Solver.Lattice
import Insieme.Solver.Metadata
import Insieme.Solver.SolverState
import Insieme.Solver.Var
import Insieme.Solver.VariableIndex
import Insieme.Solver.Statistics

-- Solver ---------------------------------------------------

-- a utility to maintain dependencies between variables
newtype Dependencies = Dependencies (IntMap (Set IndexedVar))
        deriving Show

emptyDep :: Dependencies
emptyDep = Dependencies IntMap.empty

addDep :: Dependencies -> IndexedVar -> [IndexedVar] -> Dependencies
addDep (Dependencies m) t vs = Dependencies $ IntMap.unionWith Set.union m m'
  where
    m' = IntMap.fromList $ map (\v -> (ivIndex v, Set.singleton t)) vs

getDep :: Dependencies -> IndexedVar -> Set IndexedVar
getDep (Dependencies d) v = fromMaybe Set.empty $ IntMap.lookup (ivIndex v) d

getAllDep :: Dependencies -> IndexedVar -> Set IndexedVar
getAllDep d i = collect [i] Set.empty
  where
    collect [] s = s
    collect (v:vs) s =
        let deps = getDep d v in
        let nvs = Set.difference deps s in
        collect (Set.toList nvs ++ vs) (Set.union nvs s)

-- solve for a single value
resolve :: (Lattice a) => SolverState -> TypedVar a -> (a,SolverState)
resolve i tv = (r,s)
    where
        (l,s) = resolveAll i [tv]
        r = head l

-- solve for a single value in state monad
resolveS :: (Lattice a) => TypedVar a -> State.State SolverState a
resolveS tv = do
    state <- State.get
    let (r, state') = resolve state tv
    State.put state'
    return r

-- solve for a list of variables
resolveAll :: (Lattice a) => SolverState -> [TypedVar a] -> ([a],SolverState)
resolveAll i tvs = (res <$> tvs,s)
    where
        s = solve i (toVar <$> tvs)
        ass = assignment s
        res = get' ass


-- solve for a set of variables
solve :: SolverState -> [Var] -> SolverState
solve initial vs = case violations of
        _ | not check_consistency -> res
        [] -> res
        _  -> error $ "Unsatisfied constraints:\n\t" ++ (intercalate "\n\t" $ print <$> violations )
--        _  -> trace ("Unsatisfied constraints:\n\t" ++ (intercalate "\n\t" $ print <$> violations )) res
    where
        -- compute the solution
        (ivs,nindex) = varsToIndexedVars (variableIndex initial) vs
        res = solveStep (initial {variableIndex = nindex}) emptyDep ivs

        -- compute the list of violated constraints (should be empty)
        allConstraints = concatMap constraints $ Set.toList $ knownVariables $ variableIndex res
        violations = mapMaybe (flip check $ res) allConstraints

        -- print utility formatting violations
        print v = (violationMsg v) ++ "\n\t\tshould: " ++ (violationShouldValue v)
                                   ++ "\n\t\t    is: " ++ (violationIsValue v)



-- | solve for a set of variables with an initial assignment
-- (the variable list is the work list)
-- Parameters:
--
--   * the current state (assignment and known variables)
--   * dependencies between variables
--   * work list
solveStep :: SolverState -> Dependencies -> [IndexedVar] -> SolverState

-- solveStep _ _ (q:qs) | trace ("WS-length: " ++ (show $ (length qs) + 1) ++ " next: " ++ (show q)) $ False = undefined

-- empty work list
-- solveStep s _ [] | trace (dumpToJsonFile s "ass_meta") $ False = undefined                                           -- debugging assignment as meta-info for JSON dump
-- solveStep s _ [] | trace (dumpSolverState False s "graph") False = undefined                                         -- debugging assignment as a graph plot
-- solveStep s _ [] | trace (dumpSolverState True s "graph") $ trace (dumpToJsonFile s "ass_meta") $ False = undefined  -- debugging both
-- solveStep s _ [] | trace (showSolverStatistics s) $ False = undefined                                                 -- debugging performance data
solveStep s _ [] = s                                                                                                    -- work list is empty

-- add periodic debugging messages
solveStep s _ _ | enable_tracing && trace ("Time step " ++ (show $ iterationCount s) ++ "\n" ++ (showSolverStatistics $ solverStats s Nothing)) False = undefined
  where
    enable_tracing = num_steps_between_statistic_prints > 0 && (iterationCount s) `mod` num_steps_between_statistic_prints == 0

-- compute next element in work list

solveStep (SolverState !a !i u t r s) d (v:vs) =
    solveStep (SolverState resAss resIndex nu nt nr (s+1)) resDep ds
  where
    -- profiling --
    ((resAss,resIndex,resDep,ds,numResets),dt) = measure go ()
    nt = Map.insertWith (+) aid dt t
    nu = Map.insertWith (+) aid  1 u
    nr = Map.insertWith (+) aid numResets r
    aid = analysis $ varIdent $ ivVar v

    -- ??? each constraint extends the result, the dependencies, and the vars to
    -- update

    -- | update all constraints of current variable
    go () = foldr processConstraint (a,i,d,vs,0) $ constraints (ivVar v)

    processConstraint c (a, i, d, dv, numResets) =
        case update c fa of
          -- nothing changed, we are fine
          (a', None)      -> (a', ni, nd, nv, numResets)

          -- add depending variables to work list
          (!a', Increment) -> (a', ni, nd, uv ++ nv, numResets)

          -- handling a local reset
          (a', Reset)     -> (ra, ni, nd, uv ++ nv, numResets+1)
            where
              dep = getAllDep nd trg

              !ra | not (Set.member trg dep) =
                     -- if variable is not indirectly depending on itself reset
                     -- all depending variables to their bottom value
                     reset a' dep

                 | otherwise                 =
                     -- otherwise insist on merging reseted value with current
                     -- state
                     fst $ updateWithoutReset c fa

      where
        ua = IndirectView i a
        fa = FilteredView dep a

        trg = v
        dirty_dep = dependingOn c ua
        (idep,ni) = varsToIndexedVars i dirty_dep
        dep = varToSharedVar ni <$> dirty_dep

        newVarsList = filter f idep
            where
                f iv = ivIndex iv >= numVars i

        nd = addDep d trg idep
        nv = newVarsList ++ dv

        uv = Set.elems $ getDep nd trg


--------------------------------------------------------------

-- Profiling --

measure :: (a -> b) -> a -> (b,Integer)
measure f p = unsafePerformIO $ do
        t1 <- getCPUTime
        r  <- evaluate $! f p
        t2 <- getCPUTime
        return (r,t2-t1)

-- Debugging --


-- a utility to escape double-quotes
escape :: String -> String
escape str = concat $ escape' <$> str
    where
        escape' c | c == '"' = ['\\','"']
        escape' c            = [c]


-- prints the current assignment as a graph
toDotGraph :: SolverState -> String
toDotGraph (SolverState a@(Assignment _) varIndex _ _ _ _) = "digraph G {\n\t"
        ++
        "\n\tv0 [label=\"unresolved variable!\", color=red];\n"
        ++
        -- define nodes
        ( intercalate "\n\t" ( map (\v -> "v" ++ (show $ fst v) ++ " [label=\"" ++ (show $ snd v) ++ " = " ++ (escape $ cut 100 $ valuePrint (snd v) a) ++ "\"];" ) vars ) )
        ++
        "\n\t"
        ++
        -- define edges
        ( intercalate "\n\t" ( map (\d -> "v" ++ (show $ fst d) ++ " -> v" ++ (show $ snd d) ++ ";" ) deps ) )
        ++
        "\n}"
    where

        -- get set of known variables
        varSet = knownVariables varIndex

        -- a function collecting all variables a variable is depending on
        dep v = foldr (\c l -> (dependingOn c $ UnfilteredView a) ++ l) [] (constraints v)

        -- list of all variables in the analysis
        allVars = Set.toList $ varSet

        -- the keys (=variables) associated with an index
        vars = Prelude.zip [1..] allVars

        -- a reverse lookup map for vars
        rev = Lut.fromList $ map swap vars

        -- a lookup function for rev
        index v = fromMaybe 0 $ Lut.lookup v rev

        -- computes the list of dependencies
        deps = foldr go [] vars
            where
                go = (\v l -> (foldr (\s l -> (fst v, index s) : l ) [] (dep $ snd v )) ++ l)

        -- a utility to cut the length of labels
        cut l str | length str > (l-4) = (take l str) ++ " ..."
        cut _ str = str


-- prints the current assignment to the file graph.dot and renders a pdf (for debugging)
dumpSolverState :: Bool -> I.Tree -> SolverState -> FilePath -> Bool -> String
dumpSolverState overwrite root s prefix genGraph = unsafePerformIO $ do
  evaluate $ dumpToJsonFile root s (prefix ++ "_meta")
  when genGraph $ do
    base <- solverToDot overwrite s (prefix ++ "_graph")
    pdfFromDot base
  return ("Dumped assignment to " ++ prefix ++ "* files")

-- | Dump solver state to the given file name, using the dot format.
solverToDot :: Bool -> SolverState -> FilePath -> IO FilePath
solverToDot overwrite s base = target >>=
  \f -> writeFile (f ++ ".dot") (toDotGraph s) >> return f
  where
    target = if not overwrite then nonexistFile base "dot" else return base

-- | Generate a PDF from the dot file with the given basename.
pdfFromDot :: FilePath -> IO ()
pdfFromDot b = void (system $ "dot -Tpdf " ++ b ++ ".dot -o " ++ b ++ ".pdf")


-- | Generate a file name which does not exist yet. The arguments to
-- this function is the file name base, and the file extension. The
-- returned value is the base name only.
nonexistFile :: String -> String -> IO String
nonexistFile base ext = tryFile names
    where
      tryFile []     = error "specify at least one filename"
      tryFile (f:fs) = doesFileExist (f ++ "." ++ ext)
                       >>= \e -> if e then tryFile fs else return f
      names  = base : [ iter n | n <- [ 1.. ]]
      iter n = concat [base, "-", show n]

toJsonMetaFile :: I.Tree -> SolverState -> LBS.ByteString
toJsonMetaFile root SolverState {assignment, variableIndex} = A.encode meta_file
  where
    meta_file = MetadataFile [] [] [] []
                    (Map.mapKeys I.ppNodePathStr addr_metadata_map
                            `using` parTraversable rdeepseq)

    addr_metadata_map :: Map NodePath MetadataBody
    addr_metadata_map =
        flip Map.map addr_vars_map $ \vars ->
        MetadataBody $
        flip map (Set.toList vars) $ \var ->
        let Just (MetadataVarCache _ vid fvid _np (summary, details) lims cdeps)
                = Map.lookup var var_cache
        in
        (vid,) $
        MetadataGroup fvid summary details $
        flip map (lims `zip` cdeps) $ \(limit, dep_vars) ->
        MetadataLinkGroup ("Constraint with limit " ++ limit ++ " depending on:") $
        flip map dep_vars $ \(MetadataVarCache _ dep_vid dep_fvid dep_np _ _ _) ->
        MetadataLink dep_vid dep_fvid dep_np True

    addr_vars_map :: Map NodePath (Set Var)
    addr_vars_map = Map.fromListWith Set.union $
        map (varPathJust &&& Set.singleton) $ Set.toList vars

    var_cache :: Map Var MetadataVarCache
    var_cache = Map.fromListWith dupError $ map go $ zip [0..] $ Set.toAscList vars
      where
        dupError a b = error $ "var_cache: var set duplicate, WTF?\n" ++
                       unlines [ show (mvcVar a)
                               , show (mvcVar b)
                               , show $ compare (mvcVar a) (mvcVar b)
                               ]

        go (i, v) = (v, MetadataVarCache {..})
          where
            mvcVar           = v
            mvcId            = show i
            mvcFormatted     = formatVar v
            mvcNodePath      = I.ppNodePathStr $ varPathJust v
            mvcValue         = splitSummary $ valuePrint v assignment
            (mvcConstraintLims, mvcConstrainsDeps)
                = unzip $ flip map (constraints v) $ \c ->
                     ( printLimit c (UnfilteredView assignment)
                     , map (\v -> let Just vc = Map.lookup v var_cache in vc) $
                       dependingOn c (UnfilteredView assignment)
                     )

    vars = knownVariables variableIndex

    varPathJust v = let Just x = varPath v in x
    varPath v = fmap I.getAbsolutePath $ address $ varIdent v

    hasRoot :: I.Tree -> Var -> Bool
    hasRoot r v = (I.getRoot <$> address (varIdent v)) == Just r

    formatVar var = (show (analysis vid)) ++ formatIdentifier (idValue vid)
      where
        vid = varIdent var

    formatIdentifier (IDV_NodeAddress _)
        = ""
    formatIdentifier (IDV_ProgramPoint (ProgramPoint _ p))
        = "/" ++ show p
    formatIdentifier (IDV_MemoryStatePoint (MemoryStatePoint (ProgramPoint _ p) (MemoryLocation l)))
        = "/" ++ (show p) ++ "/loc=" ++ (show l)
    formatIdentifier (IDV_Other _)
        = ""

data MetadataVarCache = MetadataVarCache
    { mvcVar           :: Var
    , mvcId            :: String
    , mvcFormatted     :: String
    , mvcNodePath      :: String
    , mvcValue         :: (String, Maybe String)
    , mvcConstraintLims:: [String]
    , mvcConstrainsDeps:: [[MetadataVarCache]]
    }

dumpToJsonFile :: I.Tree -> SolverState -> String -> String
dumpToJsonFile root s file = unsafePerformIO $ do
         LBS.writeFile (file ++ ".json") $ toJsonMetaFile root s
         return ("Dumped assignment into file " ++ file ++ ".json!")
