module Insieme.Analysis.Entities.DataPath (

    DataPath(),
    root,
    step,
    narrow,
    expand,
    
    isRoot,
    isInvalid,
    isNarrow,
    isExpand,
    getPath,
    
    append,
    invert
    
) where

import Data.List
import Insieme.Analysis.Entities.FieldIndex

--
-- * DataPaths
--


data DataPath i = 
          Root
        | Narrow [i]
        | Expand [i] 
        | Invalid
  deriving (Eq,Ord)
  

instance (Show i) => Show (DataPath i) where
    show  Root      = "⊥"
    show (Narrow p) = "⊥." ++ (intercalate "." $ show <$> (reverse p))
    show (Expand p) = (intercalate "." $ show <$> p) ++ ".⊥"
    show  Invalid   = "?"


-- a token for an empty path
root = Root

-- a token for an invalid path 
invalid = Invalid


-- creates a single-step data path
step :: i -> DataPath i
step i = Narrow [i]


narrow :: [i] -> DataPath i
narrow [] = Root
narrow p  = Narrow p


expand :: [i] -> DataPath i
expand [] = Root
expand p  = Expand p


isRoot :: DataPath i -> Bool
isRoot Root = True
isRoot _    = False


isInvalid :: DataPath i -> Bool
isInvalid Invalid = True
isInvalid _       = False

isNarrow :: DataPath i -> Bool
isNarrow  Root      = True
isNarrow (Narrow _) = True
isNarrow  _         = False 

isExpand :: DataPath i -> Bool
isExpand  Root      = True
isExpand (Expand _) = True
isExpand  _         = False 


getPath :: DataPath i -> [i]
getPath  Root      = []
getPath (Narrow p) = p
getPath (Expand p) = p


-- concatenation of paths
append :: (Eq i) => DataPath i -> DataPath i -> DataPath i
append a Root = a
append Root a = a

append Invalid _ = Invalid
append _ Invalid = Invalid

append (Narrow a) (Narrow b) = Narrow (b ++ a)
append (Expand a) (Expand b) = Expand (b ++ a)

append (Narrow [x]) (Expand [y])       | x == y = Root
append (Narrow (x:xs)) (Expand [y])    | x == y = Narrow xs
append (Narrow (x:xs)) (Expand (y:ys)) | x == y = append (Narrow xs) (Expand ys)
append (Narrow _)      (Expand _)               = Invalid

append a@(Expand _) b@(Narrow _) = invert $ append (invert a) (invert b) 



-- inverts a path from narrow to expand and vica versa
invert :: DataPath i -> DataPath i
invert  Root      = Root
invert  Invalid   = Invalid
invert (Narrow p) = Expand p
invert (Expand p) = Narrow p
