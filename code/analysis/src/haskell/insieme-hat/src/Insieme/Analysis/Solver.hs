
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
    
    -- assignments
    Assignment,
    get,
    set,
    
    -- solver
    resolve,
    solve,
    
    -- constraints
    createConstraint,
    forward,
    forwardIf,
    constant,
    
    -- debugging
    dumpSolverState
    
) where

import Debug.Trace
import Data.List
import Data.Dynamic
import Data.Typeable
import Data.Function
import Data.Tuple
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import System.Process
--import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Hashable as Hash

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
    extra    :: String,
    hash     :: Int
}
    

instance Eq Identifier where
    (==) (Identifier a1 n1 s1 h1) (Identifier a2 n2 s2 h2) =
            h1 == h2 && a1 == a2 && (getAddress n1) == (getAddress n2) && s1 == s2

instance Ord Identifier where
    compare (Identifier a1 n1 s1 h1) (Identifier a2 n2 s2 h2) =
            if r0 == EQ
               then if r1 == EQ then if r2 == EQ then r3 else r2 else r1
               else r0
        where 
            r0 = compare h1 h2
            r1 = compare a1 a2
            r2 = compare (getAddress n1) (getAddress n2)
            r3 = compare s1 s2 

instance Show Identifier where
        show (Identifier a n s _) = (show a) ++ "@" ++ (prettyShow n) ++ "/" ++ s


mkIdentifier :: AnalysisIdentifier -> NodeAddress -> String -> Identifier
mkIdentifier a n s = Identifier a n s h
  where
    h1 = Hash.hash $ idName a
    h2 = Hash.hashWithSalt h1 $ getAddress n
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


-- Debugging --

-- prints the current assignment as a graph
toDotGraph :: Assignment -> Set.Set Var -> String
toDotGraph a@( Assignment m ) varSet = "digraph G {\n\t"
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
dumpSolverState :: Assignment -> Set.Set Var -> String -> String
dumpSolverState a v file = unsafePerformIO $ do
         writeFile (file ++ ".dot") $ toDotGraph a v
         system ("dot -Tpdf " ++ file ++ ".dot -o " ++ file ++ ".pdf")
         return ("Dumped assignment into file " ++ file ++ ".pdf!")


toJsonMetaFile :: Assignment -> Set.Set Var -> String
toJsonMetaFile a@( Assignment m ) vars = "{\n"
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
                        ext = if null . extra $ i then "" else '/' : extra i
                        msg = (show . analysis $ i) ++ ext ++ 
                            " = " ++ (valuePrint v a)     
 
        print (a,ms) = "      \"" ++ (show a) ++ "\" : \"" ++ ( intercalate "<br>" ms) ++ "\""

 

dumpToJsonFile :: Assignment -> Set.Set Var -> String -> String
dumpToJsonFile a v file = unsafePerformIO $ do
         writeFile (file ++ ".json") $ toJsonMetaFile a v
         return ("Dumped assignment into file " ++ file ++ ".json!")


-- Constraints ---------------------------------------------

data Event = 
          None                        -- ^ an update had no effect
        | Increment                -- ^ an update was an incremental update
        | Reset                        -- ^ an update was not incremental



data Constraint = Constraint {
        dependingOn :: Assignment -> [Var],
        update :: (Assignment -> (Assignment,Event)),
        target :: Var
   }




-- Solver ---------------------------------------------------

-- a utility to maintain dependencies between variables
data Dependencies = Dependencies (Map.Map Var (Set.Set Var))
        deriving Show

emptyDep = Dependencies Map.empty

addDep :: Dependencies -> Var -> [Var] -> Dependencies
addDep d _ [] = d
addDep d@(Dependencies m) t (v:vs) = addDep (Dependencies (Map.insert v (Set.insert t (getDep d v)) m)) t vs

getDep :: Dependencies -> Var -> Set.Set Var
getDep (Dependencies d) v = fromMaybe Set.empty $ Map.lookup v d 


-- solve for a single value
resolve :: (Lattice a) => TypedVar a -> a
resolve tv@(TypedVar v) = get (solve empty [v]) tv

-- solve for a set of variables (with empty seed)
solve :: Assignment -> [Var] -> Assignment
solve init vs = solveStep init (Set.fromList vs) emptyDep vs


-- solve for a set of variables with an initial assignment
-- (the variable list is the work list)
-- Parameters:
--        current assignment
--        set of covered variables
--        dependencies between variables
--        work list
solveStep :: Assignment -> Set.Set Var -> Dependencies -> [Var] -> Assignment

-- debug solver state development
-- solveStep a k d wl | trace ("Step: " ++ (show wl) ++ " " ++ (show k) ++ " " ++ (show a) ++ " " ++ (show d)) False = (undefined :: Assignment)
-- solveStep a _ _ wl | trace ("Step: " ++ (show wl) ++ " " ++ (show a)) False = undefined

-- empty work list
-- solveStep a v _ [] | trace (dumpToJsonFile a v "ass_meta") $ False = undefined                                        -- debugging assignment as meta-info for JSON dump
-- solveStep a v _ [] | trace (dumpSolverState a v "graph") $ False = undefined                                          -- debugging assignment as a graph plot
-- solveStep a v _ [] | trace (dumpSolverState a v "graph") $ trace (dumpToJsonFile a v "ass_meta") $ False = undefined  -- debugging both
solveStep a _ _ [] = a                                                                                                   -- work list is empty

-- compute next element in work list
solveStep a k d (v:vs) = solveStep resAss resKnown resDep (ds ++ vs)
        where
                -- each constraint extends the result, the dependencies, and the vars to update
                (resAss,resKnown,resDep,ds) = foldr processConstraint (a,k,d,[]) ( constraints v )  -- update all constraints of current variable
                processConstraint c (a,k,d,dv) = case ( update c $ a ) of 
                        (a',None)         -> (a',nk,nd,nv)                                        -- nothing changed, we are fine
                        (a',Increment)    -> (a',nk,nd, (Set.elems $ getDep nd trg) ++ nv)        -- add depending variables to work list
                        (a',Reset)        -> undefined                                            -- TODO: support local resets
                        where
                                trg = target c                                
                                dep = dependingOn c a
                                newVars = (Set.fromList dep) `Set.difference` k
                                nk = (Set.fromList dep) `Set.union` k
                                nd = addDep d trg dep
                                nv = ( Set.elems $ newVars) ++ dv



-- Utils ---------------------------------------------------


-- a simple constraint factory
createConstraint :: (Lattice a) => ( Assignment -> [Var] ) -> ( Assignment -> a ) -> TypedVar a -> Constraint
createConstraint dep limit trg@(TypedVar var) = Constraint dep update var
        where
                update = (\a -> let value = limit a in                                                -- the value from the constraint
                                let current = get a trg in                                        -- the current value in the assignment
                                if  value `less` current then (a,None)                                 -- nothing changed
                                else ((set a trg (value `merge` current) ) , Increment)                -- an incremental change
                        )

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
