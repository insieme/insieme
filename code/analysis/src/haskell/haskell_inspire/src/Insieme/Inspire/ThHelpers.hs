module Insieme.Inspire.ThHelpers where

import Data.List
import Language.Haskell.TH

removePrefix :: String -> Name -> Name
removePrefix p n = mkName $ drop (length p) (nameBase n)

isLeaf :: Con -> Bool
isLeaf (NormalC n _) = isSuffixOf "Value" (nameBase n)
isLeaf _             = False
