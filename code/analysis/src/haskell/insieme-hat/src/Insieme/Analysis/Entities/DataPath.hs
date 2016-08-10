module Insieme.Analysis.Entities.DataPath where

import Insieme.Analysis.Entities.FieldIndex

--
-- * DataPaths
--

-- TODO: add support for negative-direction paths

data DataPath i = DataPath [i] 
  deriving (Eq,Ord)
  

instance (Show i) => Show (DataPath i) where
    show (DataPath [])     = "⊥"
    show (DataPath (x:xs)) = (show (DataPath xs)) ++ "." ++ (show x)


-- a token representing an empty path
root = DataPath []

step :: i -> DataPath i
step i = DataPath [i]

-- concatenation of paths
concatPath :: DataPath i -> DataPath i -> DataPath i
concatPath (DataPath a) (DataPath b) = DataPath $ b ++ a

-- invert paths
reversePath :: DataPath i -> DataPath i
reversePath (DataPath is) = DataPath $ reverse is
