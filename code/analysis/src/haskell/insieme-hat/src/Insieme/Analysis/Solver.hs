
module Insieme.Analysis.Solver where

import Debug.Trace
import Data.Dynamic
import Data.Maybe
--import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Hashable as Hash


-- Lattice --------------------------------------------------

class (Eq v,Typeable v) => Lattice v where
        {-# MINIMAL join #-}
        -- | combine elements
        join :: [v] -> v                -- need to be provided by implementations
        -- | binary join
        merge :: v -> v -> v                -- a binary version of the join
        merge a b = join [ a , b ]        -- its default implementation derived from join
        -- | bottom element
        bot  :: v                        -- the bottom element of the join
        bot = join []                        -- default implementation
        -- | induced order
        less :: v -> v -> Bool                -- determines whether one element of the lattice is less than another
        less a b = (a `merge` b) == b        -- default implementation


-- Identifier -----

data Identifier = Identifier Int String
        deriving (Eq, Ord)

instance Show Identifier where
        show (Identifier _ s) = s

instance Hash.Hashable Identifier where
        hashWithSalt salt (Identifier i _ ) = Hash.hashWithSalt salt i
        hash (Identifier i _ ) = i

mkIdentifier :: String -> Identifier
mkIdentifier s = Identifier (Hash.hash s) s


-- Analysis Variables ---------------------------------------

-- general variables (management)
data Var = Var {
                index :: Identifier,                 -- the variable identifier
                constraints :: [Constraint],        -- the list of constraints
                bottom :: Dynamic                -- the bottom value for this variable
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
mkVariable i cs b = TypedVar ( Var i cs ( toDyn b ) )

toVar :: TypedVar a -> Var
toVar (TypedVar x) = x


-- Assignments ----------------------------------------------

newtype Assignment = Assignment ( Map.Map Var Dynamic )
        deriving Show


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
resolve tv@(TypedVar v) = get (solve [v]) tv

-- solve for a set of variables (with empty seed)
solve :: [Var] -> Assignment
solve vs = solveStep empty (Set.fromList vs) emptyDep vs


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
solveStep a _ _ [] = a                                        -- work list is empty

-- compute next element in work list
solveStep a k d (v:vs) = solveStep resAss resKnown resDep (ds ++ vs)
        where
                -- each constraint extends the result, the dependencies, and the vars to update
                (resAss,resKnown,resDep,ds) = foldr processConstraint (a,k,d,[]) ( constraints v )  -- update all constraints of current variable
                processConstraint c (a,k,d,dv) = case ( update c $ a ) of 
                        (a',None)         -> (a',nk,nd,nv)                                        -- nothing changed, we are fine
                        (a',Increment)         -> (a',nk,nd, (Set.elems $ getDep nd trg) ++ nv)        -- add depending variables to work list
                        (a',Reset)         -> undefined                                                -- TODO: support local resets
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

-- creates a constraint of the form  x \in A[b]
constant :: (Lattice a) => a -> TypedVar a -> Constraint
constant x b = createConstraint (\_ -> []) (\a -> x) b



--------------------------------------------------------------

-- testing data

instance Lattice Bool where
        join = foldr (||) False

instance Lattice Int where
        join = foldr (+) 0


a = empty
v1 = mkVariable (mkIdentifier "v1") [] 0         :: TypedVar Int
v2 = mkVariable (mkIdentifier "v2") [] False         :: TypedVar Bool

a1 = set a v1 12
a2 = set a1 v2 True

-- example evaluation

-- this is the example analysis computing a chain of boolean values
demo :: Int -> TypedVar Bool
demo i = var
        where 
                var = mkVariable (mkIdentifier $ "v" ++ (show i)) [constraint] False
                constraint = case i of
                        0 -> constant True var                                -- terminal case
                        _ -> forward (demo (i-1)) var                        -- reference predecessor


run :: IO ()
run = print $ resolve $ demo 10

