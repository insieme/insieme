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

module Insieme.Analysis.Solver (

    -- lattices
    Lattice,
    join,
    merge,
    bot,
    less,

    ExtLattice,
    top,

    -- analysis identifiers
    AnalysisIdentifier,
    mkAnalysisIdentifier,

    -- identifiers
    Identifier,
    mkIdentifier,

    -- variables
    Var,
    TypedVar,
    mkVariable,
    toVar,
    getDependencies,
    getLimit,
    
    -- assignments
    Assignment,
    get,
    set,

    -- solver state
    SolverState,
    initState,
    assignment,

    -- solver
    resolve,
    resolveAll,
    solve,

    -- constraints
    createConstraint,
    createEqualityConstraint,
    forward,
    forwardIf,
    constant,

    -- debugging
    dumpSolverState

) where

import Control.Exception

import Debug.Trace
import Data.List
import Data.Dynamic
import Data.Function
import Data.Tuple
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import System.CPUTime
import System.Process
import Text.Printf
--import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Hashable as Hash

import qualified Data.ByteString.Char8 as BS

import Insieme.Inspire.NodeAddress


-- Lattice --------------------------------------------------

-- this is actually a bound join-semilattice
class (Eq v, Show v, Typeable v) => Lattice v where
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


class (Lattice v) => ExtLattice v where
        -- | top element
        top  :: v                           -- the top element of this lattice




-- Analysis Identifier -----

data AnalysisIdentifier = AnalysisIdentifier {
    idToken :: TypeRep,
    idName  :: String
}

instance Eq AnalysisIdentifier where
    (==) = (==) `on` idToken

instance Ord AnalysisIdentifier where
    compare = compare `on` idToken

instance Show AnalysisIdentifier where
    show = idName


mkAnalysisIdentifier :: (Typeable a) => a -> String -> AnalysisIdentifier
mkAnalysisIdentifier a n = AnalysisIdentifier (typeOf a) n


-- Identifier -----

data Identifier = Identifier {
    analysis :: AnalysisIdentifier,
    address  :: NodeAddress,
    extra    :: BS.ByteString,
    hash     :: Int
}


instance Eq Identifier where
    (==) (Identifier a1 n1 s1 h1) (Identifier a2 n2 s2 h2) =
            h1 == h2 && n1 == n2 && a1 == a2 && s1 == s2

instance Ord Identifier where
    compare (Identifier a1 n1 s1 h1) (Identifier a2 n2 s2 h2) =
            if r0 == EQ
               then if r1 == EQ then if r2 == EQ then r3 else r2 else r1
               else r0
        where
            r0 = compare h1 h2
            r1 = compare n1 n2
            r2 = compare a1 a2
            r3 = compare s1 s2

instance Show Identifier where
        show (Identifier a n s _) = (show a) ++ "@" ++ (prettyShow n) ++ "/" ++ (BS.unpack s)


mkIdentifier :: AnalysisIdentifier -> NodeAddress -> String -> Identifier
mkIdentifier a n s = Identifier a n bs h
  where
    bs = BS.pack s
    h1 = Hash.hash $ idName a
    h2 = Hash.hashWithSalt h1 $ getPathReversed n
    h = Hash.hashWithSalt h2 s


-- Analysis Variables ---------------------------------------

-- general variables (management)
data Var = Var {
                index :: Identifier,                 -- the variable identifier
                constraints :: [Constraint],         -- the list of constraints
                bottom :: Dynamic,                   -- the bottom value for this variable
                valuePrint :: Assignment -> String   -- a utility for unpacking an printing a value assigned to this variable
        }

instance Eq Var where
        (==) a b = (index a) == (index b)

instance Ord Var where
        compare a b = compare (index a) (index b)

instance Show Var where
        show v = show (index v)


-- typed variables (user interaction)
newtype TypedVar a = TypedVar Var
        deriving ( Show, Eq, Ord )

mkVariable :: (Lattice a) => Identifier -> [Constraint] -> a -> TypedVar a
mkVariable i cs b = var
    where
        var = TypedVar ( Var i cs ( toDyn b ) print )
        print = (\a -> show $ get a var )

toVar :: TypedVar a -> Var
toVar (TypedVar x) = x


getDependencies :: Assignment -> TypedVar a -> [Var]
getDependencies a v = concat $ (go <$> (constraints . toVar) v)
    where
        go c = dependingOn c a 

getLimit :: (Lattice a) => Assignment -> TypedVar a -> a
getLimit a v = join (go <$> (constraints . toVar) v)
    where
        go c = get a' v
            where
                (a',_) = update c a 


-- Assignments ----------------------------------------------

newtype Assignment = Assignment ( Map.Map Var Dynamic )

instance Show Assignment where
    show a@( Assignment m ) = "Assignemnet {\n\t"
            ++
            ( intercalate ",\n\t\t" ( map (\v -> (show v) ++ " = " ++ (valuePrint v a) ) vars ) )
            ++
            "\n}"
        where
            vars = Map.keys m


empty :: Assignment
empty = Assignment Map.empty

-- retrieves a value from the assignment
-- if the value is not present, the bottom value of the variable will be returned
get :: (Typeable a) => Assignment -> TypedVar a -> a
get (Assignment m) (TypedVar v) =
        fromJust $ (fromDynamic :: ((Typeable a) => Dynamic -> (Maybe a)) ) $ fromMaybe (bottom v) (Map.lookup v m)

-- updates the value for the given variable stored within the given assignment
set :: (Typeable a) => Assignment -> TypedVar a -> a -> Assignment
set (Assignment a) (TypedVar v) d = Assignment (Map.insert v (toDyn d) a)


-- resets the values of the given variables within the given assignment
reset :: Assignment -> Set.Set Var -> Assignment
reset (Assignment a) vars = Assignment $ Map.union reseted a
    where 
        reseted = Map.fromList (go <$> Set.toList vars)
        go v = (v,bottom v)
 

-- Constraints ---------------------------------------------

data Event =
          None                        -- ^ an update had no effect
        | Increment                   -- ^ an update was an incremental update
        | Reset                       -- ^ an update was not incremental



data Constraint = Constraint {
        dependingOn         :: Assignment -> [Var],
        update              :: (Assignment -> (Assignment,Event)),       -- update the assignment, a reset is allowed       
        updateWithoutReset  :: (Assignment -> (Assignment,Event)),       -- update the assignment, a reset is not allowed
        target              :: Var
   }




-- Solver ---------------------------------------------------

-- a aggregation of the 'state' of a solver for incremental analysis

data SolverState = SolverState {
        assignment :: Assignment,
        knownVariables :: Set.Set Var,         
        -- for performance evaluation
        numSteps :: Map.Map AnalysisIdentifier Int,                     
        cpuTimes :: Map.Map AnalysisIdentifier Integer
    } 

initState = SolverState empty Set.empty Map.empty Map.empty


-- a utility to maintain dependencies between variables
data Dependencies = Dependencies (Map.Map Var (Set.Set Var))
        deriving Show

emptyDep = Dependencies Map.empty

addDep :: Dependencies -> Var -> [Var] -> Dependencies
addDep d _ [] = d
addDep d@(Dependencies m) t (v:vs) = addDep (Dependencies (Map.insertWith (\_ s -> Set.insert t s) v (Set.singleton t) m)) t vs

getDep :: Dependencies -> Var -> Set.Set Var
getDep (Dependencies d) v = fromMaybe Set.empty $ Map.lookup v d

getAllDep :: Dependencies -> Var -> Set.Set Var
getAllDep d v = collect d [v] Set.empty
    where
        collect d [] s = s
        collect d (v:vs) s = collect d ((Set.toList $ Set.difference dep s) ++ vs) (Set.union dep s)
            where
                dep = getDep d v 


-- solve for a single value
resolve :: (Lattice a) => SolverState -> TypedVar a -> (a,SolverState)
resolve i tv = (r,s)
    where
        (l,s) = resolveAll i [tv]
        r = head l


-- solve for a list of variables
resolveAll :: (Lattice a) => SolverState -> [TypedVar a] -> ([a],SolverState)
resolveAll i tvs = (res <$> tvs,s)
    where
        s = solve i (toVar <$> tvs)
        ass = assignment s 
        res = get ass


-- solve for a set of variables
solve :: SolverState -> [Var] -> SolverState
solve init vs = solveStep (init {knownVariables = known_vars}) emptyDep vs
    where
        known_vars = Set.union (Set.fromList vs) (knownVariables init)  


-- solve for a set of variables with an initial assignment
-- (the variable list is the work list)
-- Parameters:
--        the current state (assignment and known variables)
--        dependencies between variables
--        work list
solveStep :: SolverState -> Dependencies -> [Var] -> SolverState


-- solveStep _ _ (q:qs) | trace ("WS-length: " ++ (show $ (length qs) + 1) ++ " next: " ++ (show q)) $ False = undefined

-- empty work list
-- solveStep s _ [] | trace (dumpToJsonFile s "ass_meta") $ False = undefined                                           -- debugging assignment as meta-info for JSON dump
-- solveStep s _ [] | trace (dumpSolverState s "graph") $ False = undefined                                             -- debugging assignment as a graph plot
-- solveStep s _ [] | trace (dumpSolverState s "graph") $ trace (dumpToJsonFile s "ass_meta") $ False = undefined       -- debugging both
-- solveStep s _ [] | trace (showSolverStatistic s) $ False = undefined                                                 -- debugging performance data
solveStep s _ [] = s                                                                                                    -- work list is empty

-- compute next element in work list
solveStep (SolverState a k i t) d (v:vs) = solveStep (SolverState resAss resKnown ni nt) resDep ds
        where
                -- profiling --
                ((resAss,resKnown,resDep,ds),dt) = measure go ()
                nt = Map.insertWith (+) aid dt t
                ni = Map.insertWith (+) aid  1 i
                aid = analysis $ index v
                
                -- each constraint extends the result, the dependencies, and the vars to update
                go _ = foldr processConstraint (a,k,d,vs) ( constraints v )  -- update all constraints of current variable
                processConstraint c (a,k,d,dv) = case ( update c a ) of
                        
                        (a',None)         -> (a',nk,nd,nv)                                        -- nothing changed, we are fine
                        
                        (a',Increment)    -> (a',nk,nd, (Set.elems $ getDep nd trg) ++ nv)        -- add depending variables to work list
                        
                        (a',Reset)        -> (ra,nk,nd, (Set.elems $ getDep nd trg) ++ nv)        -- handling a local reset
                            where 
                                dep = getAllDep nd trg
                                ra = if not $ Set.member trg dep                                  -- if variable is not indirectly depending on itself  
                                    then reset a' dep                                             -- reset all depending variables to their bottom value 
                                    else fst $ updateWithoutReset c a                             -- otherwise insist on merging reseted value with current state
                                        
                    where
                            trg = target c
                            dep = dependingOn c a
                            newVars = (Set.fromList dep) `Set.difference` k
                            nk = newVars `Set.union` k
                            nd = addDep d trg dep
                            nv = ( Set.elems $ newVars) ++ dv



-- Utils ---------------------------------------------------


-- | A simple constraint factory, taking as arguments
--   * a function to return the dependent variables of this constraint,
--   * the current value of the constraint,
--   * and the target variable for this constraint.
createConstraint :: (Lattice a) => ( Assignment -> [Var] ) -> ( Assignment -> a ) -> TypedVar a -> Constraint
createConstraint dep limit trg@(TypedVar var) = Constraint dep update update var
    where
        update a = case () of
                _ | value `less` current -> (                                a,      None)    -- nothing changed                                    
                _                        -> (set a trg (value `merge` current), Increment)    -- an incremental change                              
            where 
                value = limit a                                                               -- the value from the constraint      
                current = get a trg                                                           -- the current value in the assignment

                
-- creates a constraint of the form f(A) = A[b] enforcing equality
createEqualityConstraint dep limit trg@(TypedVar var) = Constraint dep update forceUpdate var
    where
        update a = case () of
                _ | value `less` current -> (              a,      None)    -- nothing changed                                    
                _ | current `less` value -> (set a trg value, Increment)    -- an incremental change                              
                _                        -> (set a trg value,     Reset)    -- a reseting change, heading in a different direction
            where 
                value = limit a                                             -- the value from the constraint      
                current = get a trg                                         -- the current value in the assignment

        forceUpdate a = case () of
                _ | value `less` current -> (                                a,      None)    -- nothing changed                                    
                _                        -> (set a trg (value `merge` current), Increment)    -- an incremental change                              
            where 
                value = limit a                                                               -- the value from the constraint      
                current = get a trg                                                           -- the current value in the assignment



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
constant x b = createConstraint (\_ -> []) (\a -> x) b



--------------------------------------------------------------

-- Profiling --

measure :: (a -> b) -> a -> (b,Integer)
measure f p = unsafePerformIO $ do    
        t1 <- getCPUTime
        r  <- evaluate $! f p
        t2 <- getCPUTime 
        return (r,t2-t1)


-- Debugging --

-- prints the current assignment as a graph
toDotGraph :: SolverState -> String
toDotGraph (SolverState a@( Assignment m ) varSet _ _) = "digraph G {\n\t"
        ++
        "\n\tv0 [label=\"unresolved variable!\", color=red];\n"
        ++
        -- define nodes
        ( intercalate "\n\t" ( map (\v -> "v" ++ (show $ fst v ) ++ " [label=\"" ++ (show $ snd v) ++ " = " ++ (valuePrint (snd v) a) ++ "\"];" ) vars ) )
        ++
        "\n\t"
        ++
        -- define edges
        ( intercalate "\n\t" ( map (\d -> "v" ++ (show $ fst d) ++ " -> v" ++ (show $ snd d) ++ ";" ) deps ) )
        ++
        "\n}"
    where

        -- a function collecting all variables a variable is depending on
        dep v = foldr (\c l -> (dependingOn c a) ++ l) [] (constraints v)

        -- list of all keys in map
        keys = Map.keys m

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


-- prints the current assignment to the file graph.dot and renders a pdf (for debugging)
dumpSolverState :: SolverState -> String -> String
dumpSolverState s file = unsafePerformIO $ do
         writeFile (file ++ ".dot") $ toDotGraph s
         system ("dot -Tpdf " ++ file ++ ".dot -o " ++ file ++ ".pdf")
         return ("Dumped assignment into file " ++ file ++ ".pdf!")


toJsonMetaFile :: SolverState -> String
toJsonMetaFile (SolverState a@( Assignment m ) vars _ _) = "{\n"
        ++
        "    \"bodies\": {\n"
        ++
        ( intercalate ",\n" ( map print $ Map.toList store ) )
        ++
        "\n    }\n}"
    where

        addr = address . index

        store = foldr go Map.empty vars
            where
                go v m = Map.insert k (msg : Map.findWithDefault [] k m) m
                    where
                        k = (addr v)
                        i = index v
                        s = BS.unpack $ extra i
                        ext = if null s then "" else '/' : s
                        msg = (show . analysis $ i) ++ ext ++
                            " = " ++ (valuePrint v a)

        print (a,ms) = "      \"" ++ (show a) ++ "\" : \"" ++ ( intercalate "<br>" ms) ++ "\""



dumpToJsonFile :: SolverState -> String -> String
dumpToJsonFile s file = unsafePerformIO $ do
         writeFile (file ++ ".json") $ toJsonMetaFile s
         return ("Dumped assignment into file " ++ file ++ ".json!")


showSolverStatistic :: SolverState -> String
showSolverStatistic s = 
        "==================================================== Solver Statistic ====================================================\n" ++
        "       Analysis                #Vars              Updates          Updates/Var            ~Time[us]        ~Time/Var[us]" ++
        "\n==========================================================================================================================\n" ++
        ( intercalate "\n" (map print $ Map.toList grouped)) ++
        "\n--------------------------------------------------------------------------------------------------------------------------\n" ++
        "         Total: " ++ (printf "%20d" numVars) ++ 
                        (printf " %20d" totalUpdates) ++ (printf " %20.3f" avgUpdates) ++ 
                        (printf " %20d" totalTime) ++ (printf " %20.3f" avgTime) ++
        "\n==========================================================================================================================\n"
    where
        vars = knownVariables s
        
        grouped = foldr go Map.empty vars
            where
                go v m = Map.insertWith (+) ( analysis . index $ v ) (1::Int) m
        
        print (a,c) = printf "     %10s %20d %20d %20.3f %20d %20.3f" name c totalUpdates avgUpdates totalTime avgTime 
            where
                name = ((show a) ++ ":")
            
                totalUpdates = Map.findWithDefault 0 a $ numSteps s
                avgUpdates = ((fromIntegral totalUpdates) / (fromIntegral c)) :: Float
            
                totalTime = toMs $ Map.findWithDefault 0 a $ cpuTimes s
                avgTime = ((fromIntegral totalTime) / (fromIntegral c)) :: Float


        numVars = Set.size vars
        
        totalUpdates = foldr (+) 0 (numSteps s)
        avgUpdates = ((fromIntegral totalUpdates) / (fromIntegral numVars)) :: Float
        
        totalTime = toMs $ foldr (+) 0 (cpuTimes s)
        avgTime = ((fromIntegral totalTime) / (fromIntegral numVars)) :: Float
        
        toMs a = a `div` 1000000