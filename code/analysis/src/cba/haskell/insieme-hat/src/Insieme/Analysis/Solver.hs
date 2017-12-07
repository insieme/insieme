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

module Insieme.Analysis.Solver (

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
    showSolverStatistic,

    -- metadata
    module Insieme.Analysis.Solver.Metadata

) where

import GHC.Stack

import Prelude hiding (lookup,print)
import Control.Arrow
import Control.DeepSeq
import Control.Exception
import Control.Monad (void,when)
import Control.Parallel.Strategies
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Dynamic
import Data.Function
import Data.List hiding (insert,lookup)
import Data.Maybe
import Data.Tuple
import System.CPUTime
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Printf

import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Graph as Graph
import qualified Data.Hashable as Hash
import qualified Data.IntMap.Strict as IntMap
import           Data.Set (Set)
import qualified Data.Set as Set

import Insieme.Inspire (NodeAddress, NodePath)
import qualified Insieme.Inspire as I
import Insieme.Utils

import Insieme.Analysis.Entities.Memory
import Insieme.Analysis.Entities.ProgramPoint

import Insieme.Analysis.Solver.Metadata


-- A flag to enable / disable internal consistency checks (for debugging)
check_consistency :: Bool
check_consistency = False


-- Lattice --------------------------------------------------

-- this is actually a bound join-semilattice
class (Eq v, Show v, Typeable v, NFData v) => Lattice v where
        {-# MINIMAL join | merge, bot #-}
        -- | combine elements
        join :: [v] -> v                    -- need to be provided by implementations
        join [] = bot                       -- a default implementation for the join operator
        join xs = foldr1 merge xs           -- a default implementation for the join operator

        -- | binary join
        merge :: v -> v -> v                -- a binary version of the join
        merge a b = join [ a , b ]          -- its default implementation derived from join

        -- | bottom element
        bot  :: v                           -- the bottom element of the join
        bot = join []                       -- default implementation

        -- | induced order
        less :: v -> v -> Bool              -- determines whether one element of the lattice is less than another
        less a b = (a `merge` b) == b       -- default implementation

        -- | debug printing
        print :: v -> String                -- print a value of the lattice readable
        print = show



class (Lattice v) => ExtLattice v where
        -- | top element
        top  :: v                           -- the top element of this lattice




-- Analysis Identifier -----

data AnalysisIdentifier = AnalysisIdentifier {
    aidToken :: TypeRep,
    aidName  :: String,
    aidHash  :: Int
}

instance Eq AnalysisIdentifier where
    (==) = (==) `on` aidToken

instance Ord AnalysisIdentifier where
    compare = compare `on` aidToken

instance Show AnalysisIdentifier where
    show = aidName


mkAnalysisIdentifier :: (Typeable a) => a -> String -> AnalysisIdentifier
mkAnalysisIdentifier a n = AnalysisIdentifier {
        aidToken = (typeOf a), aidName = n , aidHash = (Hash.hash n)
    }


-- Identifier -----


data IdentifierValue =
          IDV_NodeAddress NodeAddress
        | IDV_ProgramPoint ProgramPoint
        | IDV_MemoryStatePoint MemoryStatePoint
        | IDV_Other BS.ByteString
    deriving (Eq,Ord,Show)


referencedAddress :: IdentifierValue -> Maybe NodeAddress
referencedAddress ( IDV_NodeAddress   a )                                           = Just a
referencedAddress ( IDV_ProgramPoint (ProgramPoint a _) )                           = Just a
referencedAddress ( IDV_MemoryStatePoint (MemoryStatePoint (ProgramPoint a _) _ ) ) = Just a
referencedAddress ( IDV_Other _ )                                                   = Nothing



data Identifier = Identifier {
    idHash   :: Int,
    idValue  :: IdentifierValue,
    analysis :: AnalysisIdentifier
} deriving (Eq, Ord)

instance Show Identifier where
        show (Identifier a v _) = (show a) ++ "/" ++ (show v)


mkIdentifierFromExpression :: AnalysisIdentifier -> NodeAddress -> Identifier
mkIdentifierFromExpression a n = Identifier {
        analysis = a,
        idValue = IDV_NodeAddress n,
        idHash = Hash.hashWithSalt (aidHash a) n
    }

mkIdentifierFromProgramPoint :: AnalysisIdentifier -> ProgramPoint -> Identifier
mkIdentifierFromProgramPoint a p = Identifier {
    analysis = a,
    idValue = IDV_ProgramPoint p,
    idHash = Hash.hashWithSalt (aidHash a) p
}

mkIdentifierFromMemoryStatePoint :: AnalysisIdentifier -> MemoryStatePoint -> Identifier
mkIdentifierFromMemoryStatePoint a m = Identifier {
    analysis = a,
    idValue = IDV_MemoryStatePoint m,
    idHash = Hash.hashWithSalt (aidHash a) m
}

mkIdentifierFromString :: AnalysisIdentifier -> String -> Identifier
mkIdentifierFromString a s = Identifier {
    analysis = a,
    idValue = IDV_Other $ BS.pack s,
    idHash = Hash.hashWithSalt (aidHash a) $ s
}

address :: Identifier -> Maybe NodeAddress
address = referencedAddress . idValue



-- Analysis Variables ---------------------------------------

-- general variables (management)
data Var = Var {
                varIdent :: Identifier,              -- the variable identifier
                constraints :: [Constraint],         -- the list of constraints
                bottom :: Dynamic,                   -- the bottom value for this variable
                valuePrint :: Assignment -> String   -- a utility for unpacking an printing a value assigned to this variable
        }

instance Eq Var where
        (==) a b = (varIdent a) == (varIdent b)

instance Ord Var where
        compare a b = compare (varIdent a) (varIdent b)

instance Show Var where
        show v = show (varIdent v)

-- typed variables (user interaction)
newtype TypedVar a = TypedVar Var
        deriving ( Show, Eq, Ord )

mkVariable :: (Lattice a) => Identifier -> [Constraint] -> a -> TypedVar a
mkVariable i cs b = var
    where
        var = TypedVar ( Var i cs ( toDyn b ) print' )
        print' = (\a -> print $ get' a var )

toVar :: TypedVar a -> Var
toVar (TypedVar x) = x

getDependencies :: AssignmentView -> TypedVar a -> [Var]
getDependencies a v = concat $ (go <$> (constraints . toVar) v)
    where
        go c = dependingOn c a

getLimit :: (Lattice a) => AssignmentView -> TypedVar a -> a
getLimit a v = join (go <$> (constraints . toVar) v)
    where
        go c = get' a' v
            where
                (a',_) = update c a



-- Analysis Variable Maps -----------------------------------

newtype VarMap a = VarMap (IntMap.IntMap (Map.Map Var a))

emptyVarMap :: VarMap a
emptyVarMap = VarMap IntMap.empty

lookup :: Var -> VarMap a -> Maybe a
lookup k (VarMap m) = (Map.lookup k) =<< (IntMap.lookup (idHash $ varIdent k) m)

insert :: Var -> a -> VarMap a -> VarMap a
insert k v (VarMap m) = VarMap (IntMap.insertWith go (idHash $ varIdent k) (Map.singleton k v) m)
    where
        go _ o = Map.insert k v o

insertAll :: [(Var,a)] -> VarMap a -> VarMap a
insertAll [] m = m
insertAll ((k,v):xs) m = insertAll xs $ insert k v m


keys :: VarMap a -> [Var]
keys (VarMap m) = foldr go [] m
    where
        go im l = (Map.keys im) ++ l


keysSet :: VarMap a -> Set.Set Var
keysSet (VarMap m) = foldr go Set.empty m
    where
        go im s = Set.union (Map.keysSet im) s


-- Assignments ----------------------------------------------

newtype Assignment = Assignment ( VarMap Dynamic )

instance Show Assignment where
    show a@( Assignment m ) = "Assignment {\n\t"
            ++
            ( intercalate ",\n\t\t" ( map (\v -> (show v) ++ " #" ++ (show $ idHash $ varIdent v) ++ " = " ++ (valuePrint v a) ) vars ) )
            ++
            "\n}"
        where
            vars = keys m


empty :: Assignment
empty = Assignment emptyVarMap

-- retrieves a value from the assignment
-- if the value is not present, the bottom value of the variable will be returned
get' :: (Typeable a) => Assignment -> TypedVar a -> a
get' (Assignment m) (TypedVar v) =
        fromJust $ (fromDynamic :: ((Typeable a) => Dynamic -> (Maybe a)) ) $ fromMaybe (bottom v) (lookup v m)


-- updates the value for the given variable stored within the given assignment
set :: (Typeable a, NFData a) => Assignment -> TypedVar a -> a -> Assignment
set (Assignment a) (TypedVar v) d = Assignment (insert v (toDyn d) a)


-- resets the values of the given variables within the given assignment
reset :: Assignment -> Set.Set IndexedVar -> Assignment
reset (Assignment m) vars = Assignment $ insertAll reseted m
    where
        reseted = go <$> Set.toList vars
        go iv = (v,bottom v)
            where
                v = indexToVar iv


-- Assignment Views ----------------------------------------

data AssignmentView = UnfilteredView Assignment
                    | FilteredView [Var] Assignment

get :: (HasCallStack, Typeable a) => AssignmentView -> TypedVar a -> a
get (UnfilteredView a) v = get' a v
get (FilteredView vs a) v = case () of 
    _ | not check_consistency || elem (toVar v) vs -> res
    _ | otherwise -> error ("Invalid variable access: " ++ (show v) ++ " not in " ++ (show vs))
  where
    res = get' a v

stripFilter :: AssignmentView -> Assignment
stripFilter (UnfilteredView a) = a
stripFilter (FilteredView _ a) = a


-- Constraints ---------------------------------------------

data Event =
          None                        -- ^ an update had no effect
        | Increment                   -- ^ an update was an incremental update
        | Reset                       -- ^ an update was not incremental



data Constraint = Constraint {
        dependingOn         :: AssignmentView -> [Var],                      -- obtains list of variables depending on
        update              :: (AssignmentView -> (Assignment,Event)),       -- update the assignment, a reset is allowed
        updateWithoutReset  :: (AssignmentView -> (Assignment,Event)),       -- update the assignment, a reset is not allowed
        check               :: SolverState -> Maybe Violation,               -- check whether this constraint is satisfied
        printLimit          :: AssignmentView -> String                      -- requests to print the current limit defined (debugging)
   }


data Violation = Violation {
        violationMsg         :: String,       -- a message describing the issue
        violationShouldValue :: String,       -- the value a variable should have (at least)
        violationIsValue     :: String        -- the value a variable does have
    }


-- Variable Index -----------------------------------------------


-- a utility for indexing variables
data IndexedVar = IndexedVar Int Var


indexToVar :: IndexedVar -> Var
indexToVar (IndexedVar _ v) = v

toIndex :: IndexedVar -> Int
toIndex (IndexedVar i _ ) = i


instance Eq IndexedVar where
    (IndexedVar a _) == (IndexedVar b _) = a == b

instance Ord IndexedVar where
    compare (IndexedVar a _) (IndexedVar b _) = compare a b

instance Show IndexedVar where
    show (IndexedVar _ v) = show v



data VariableIndex = VariableIndex Int (VarMap IndexedVar)

emptyVarIndex :: VariableIndex
emptyVarIndex = VariableIndex 0 emptyVarMap

numVars :: VariableIndex -> Int
numVars (VariableIndex n _) = n

knownVariables :: VariableIndex -> Set.Set Var
knownVariables (VariableIndex _ m) = keysSet m

varToIndex :: VariableIndex -> Var -> (IndexedVar,VariableIndex)
varToIndex (VariableIndex n m) v = (res, VariableIndex nn nm)
    where

        ri = lookup v m

        nm = if isNothing ri then insert v ni m else m

        ni = IndexedVar n v           -- the new indexed variable, if necessary

        res = fromMaybe ni ri

        nn = if isNothing ri then n+1 else n


varsToIndex :: VariableIndex -> [Var] -> ([IndexedVar],VariableIndex)
varsToIndex i vs = foldr go ([],i) vs
    where
        go v (rs,i') = (r:rs,i'')
            where
                (r,i'') = varToIndex i' v



-- Solver ---------------------------------------------------

-- a aggregation of the 'state' of a solver for incremental analysis

data SolverState = SolverState {
        assignment :: Assignment,
        variableIndex :: VariableIndex,
        -- for performance evaluation
        numSteps  :: Map.Map AnalysisIdentifier Int,
        cpuTimes  :: Map.Map AnalysisIdentifier Integer,
        numResets :: Map.Map AnalysisIdentifier Int
    }

initState :: SolverState
initState = SolverState empty emptyVarIndex Map.empty Map.empty Map.empty



-- a utility to maintain dependencies between variables
data Dependencies = Dependencies (IntMap.IntMap (Set.Set IndexedVar))
        deriving Show

emptyDep :: Dependencies
emptyDep = Dependencies IntMap.empty

addDep :: Dependencies -> IndexedVar -> [IndexedVar] -> Dependencies
addDep d _ [] = d
addDep (Dependencies m) t (v:vs) = addDep (Dependencies (IntMap.insertWith (\_ s -> Set.insert t s) (toIndex v) (Set.singleton t) m)) t vs

getDep :: Dependencies -> IndexedVar -> Set.Set IndexedVar
getDep (Dependencies d) v = fromMaybe Set.empty $ IntMap.lookup (toIndex v) d

getAllDep :: Dependencies -> IndexedVar -> Set.Set IndexedVar
getAllDep d i = collect [i] Set.empty
    where
        collect [] s = s
        collect (v:vs) s = collect ((Set.toList $ Set.difference dep s) ++ vs) (Set.union dep s)
            where
                dep = getDep d v


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
        (ivs,nindex) = varsToIndex (variableIndex initial) vs
        res = solveStep (initial {variableIndex = nindex}) emptyDep ivs

        -- compute the list of violated constraints (should be empty)
        allConstraints = concatMap constraints $ Set.toList $ knownVariables $ variableIndex res
        violations = mapMaybe (flip check $ res) allConstraints

        -- print utility formatting violations
        print v = (violationMsg v) ++ "\n\t\tshould: " ++ (violationShouldValue v) 
                                   ++ "\n\t\t    is: " ++ (violationIsValue v)



-- solve for a set of variables with an initial assignment
-- (the variable list is the work list)
-- Parameters:
--        the current state (assignment and known variables)
--        dependencies between variables
--        work list
solveStep :: SolverState -> Dependencies -> [IndexedVar] -> SolverState


-- solveStep _ _ (q:qs) | trace ("WS-length: " ++ (show $ (length qs) + 1) ++ " next: " ++ (show q)) $ False = undefined

-- empty work list
-- solveStep s _ [] | trace (dumpToJsonFile s "ass_meta") $ False = undefined                                           -- debugging assignment as meta-info for JSON dump
-- solveStep s _ [] | trace (dumpSolverState False s "graph") False = undefined                                         -- debugging assignment as a graph plot
-- solveStep s _ [] | trace (dumpSolverState True s "graph") $ trace (dumpToJsonFile s "ass_meta") $ False = undefined  -- debugging both
-- solveStep s _ [] | trace (showSolverStatistic s) $ False = undefined                                                 -- debugging performance data
solveStep s _ [] = s                                                                                                    -- work list is empty

-- compute next element in work list
solveStep (SolverState a i u t r) d (v:vs) = solveStep (SolverState resAss resIndex nu nt nr) resDep ds
        where
                -- profiling --
                ((resAss,resIndex,resDep,ds,numResets),dt) = measure go ()
                nt = Map.insertWith (+) aid dt t
                nu = Map.insertWith (+) aid  1 u
                nr = if numResets > 0 then Map.insertWith (+) aid numResets r else r
                aid = analysis $ varIdent $ indexToVar v

                -- each constraint extends the result, the dependencies, and the vars to update
                go _ = foldr processConstraint (a,i,d,vs,0) ( constraints $ indexToVar v )  -- update all constraints of current variable
                processConstraint c (a,i,d,dv,numResets) = case ( update c fa ) of

                        (a',None)         -> (a',ni,nd,nv,numResets)                -- nothing changed, we are fine

                        (a',Increment)    -> (a',ni,nd, uv ++ nv, numResets)        -- add depending variables to work list

                        (a',Reset)        -> (ra,ni,nd, uv ++ nv, numResets+1)      -- handling a local reset
                            where
                                dep = getAllDep nd trg
                                ra = if not $ Set.member trg dep                    -- if variable is not indirectly depending on itself
                                    then reset a' dep                               -- reset all depending variables to their bottom value
                                    else fst $ updateWithoutReset c fa              -- otherwise insist on merging reseted value with current state

                    where
                            ua = UnfilteredView a
                            fa = FilteredView dep a

                            trg = v
                            dep = dependingOn c ua
                            (idep,ni) = varsToIndex i dep

                            newVarsList = filter f idep
                                where
                                    f iv = toIndex iv >= numVars i

                            nd = addDep d trg idep
                            nv = newVarsList ++ dv

                            uv = Set.elems $ getDep nd trg



-- Utils ---------------------------------------------------


-- | A simple constraint factory, taking as arguments
--   * a function to return the dependent variables of this constraint,
--   * the current value of the constraint,
--   * and the target variable for this constraint.
createConstraint :: (Lattice a) => ( AssignmentView -> [Var] ) -> ( AssignmentView -> a ) -> TypedVar a -> Constraint
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
forward :: (Lattice a) => TypedVar a -> TypedVar a -> Constraint
forward a@(TypedVar v) b = createConstraint (\_ -> [v]) (\a' -> get a' a) b


-- creates a constraint of the form  x \sub A[a] => A[b] \in A[c]
forwardIf :: (Lattice a, Lattice b) => a -> TypedVar a -> TypedVar b -> TypedVar b -> Constraint
forwardIf a b@(TypedVar v1) c@(TypedVar v2) d = createConstraint dep upt d
    where
        dep = (\a' -> if less a $ get a' b then [v1,v2] else [v1] )
        upt = (\a' -> if less a $ get a' b then get a' c else bot )


-- creates a constraint of the form  x \in A[b]
constant :: (Lattice a) => a -> TypedVar a -> Constraint
constant x b = createConstraint (const []) (const x) b



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
toDotGraph (SolverState a@(Assignment _) varIndex _ _ _) = "digraph G {\n\t"
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
        rev = Map.fromList $ map swap vars

        -- a lookup function for rev
        index v = fromMaybe 0 $ Map.lookup v rev

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


showSolverStatistic :: SolverState -> String
showSolverStatistic s =
        "========================================================= Solver Statistic ==============================================================================================\n" ++
        "             Analysis                #Vars              Updates          Updates/Var            ~Time[us]        ~Time/Var[us]               Resets           Resets/Var" ++
        "\n=========================================================================================================================================================================\n" ++
        ( intercalate "\n" (map print $ Map.toList grouped)) ++
        "\n-------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n" ++
        "               Total: " ++ (printf "%20d" numVars) ++
                        (printf " %20d" totalUpdates) ++ (printf " %20.3f" avgUpdates) ++
                        (printf " %20d" totalTime) ++ (printf " %20.3f" avgTime) ++
                        (printf " %20d" totalResets) ++ (printf " %20.3f" avgResets) ++
        "\n=========================================================================================================================================================================\n" ++
        analyseVarDependencies s ++
        "\n=========================================================================================================================================================================\n"
    where
        vars = knownVariables $ variableIndex s

        grouped = foldr go Map.empty vars
            where
                go v m = Map.insertWith (+) ( analysis . varIdent $ v ) (1::Int) m

        print (a,c) = printf " %20s %20d %20d %20.3f %20d %20.3f %20d %20.3f" name c totalUpdates avgUpdates totalTime avgTime totalResets avgResets
            where
                name = ((show a) ++ ":")

                totalUpdates = Map.findWithDefault 0 a $ numSteps s
                avgUpdates = ((fromIntegral totalUpdates) / (fromIntegral c)) :: Float

                totalTime = toMs $ Map.findWithDefault 0 a $ cpuTimes s
                avgTime = ((fromIntegral totalTime) / (fromIntegral c)) :: Float

                totalResets = Map.findWithDefault 0 a $ numResets s
                avgResets = ((fromIntegral totalResets) / (fromIntegral c)) :: Float


        numVars = Set.size vars

        totalUpdates = foldr (+) 0 (numSteps s)
        avgUpdates = ((fromIntegral totalUpdates) / (fromIntegral numVars)) :: Float

        totalTime = toMs $ foldr (+) 0 (cpuTimes s)
        avgTime = ((fromIntegral totalTime) / (fromIntegral numVars)) :: Float

        totalResets = foldr (+) 0 (numResets s)
        avgResets = ((fromIntegral totalResets) / (fromIntegral numVars)) :: Float

        toMs a = a `div` 1000000


analyseVarDependencies :: SolverState -> String
analyseVarDependencies s =

            "  Number of SCCs: " ++ show (length sccs)   ++ "\n" ++
            "  largest SCCs:   " ++ show (take 10 $ reverse $ sort $ length . Graph.flattenSCC <$> sccs)

    where

        ass = assignment s

        index = variableIndex s

        vars = knownVariables index

        -- getDep :: Dependencies -> IndexedVar -> Set.Set IndexedVar

        -- varToIndex :: VariableIndex -> Var -> (IndexedVar,VariableIndex)

        nodes = go <$> Set.toList vars
            where
                go v = (v,v,dep v)
                dep v = foldr (\c l -> (dependingOn c $ UnfilteredView ass) ++ l) [] (constraints v)


        sccs = Graph.stronglyConnComp nodes


