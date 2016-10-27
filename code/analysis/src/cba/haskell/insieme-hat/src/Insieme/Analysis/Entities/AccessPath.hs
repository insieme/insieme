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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Insieme.Analysis.Entities.AccessPath (

    BaseVar(..),

    AccessPath(..),
    unknown,
    parameter,
    global,
    local,
    
    append,
    deref,
    extend

) where

import Control.DeepSeq
import Data.List
import GHC.Generics (Generic)
import qualified Insieme.Analysis.Entities.DataPath as DP
import qualified Insieme.Inspire as IR


--
-- * AccessPaths
--

data BaseVar =
          Parameter Int
        | Global IR.Tree
    deriving (Eq,Ord,Generic,NFData)

instance Show BaseVar where
    show (Parameter i) = "p" ++ (show i)
    show (Global n)    = show n 

data AccessPath i = 
          AccessPath BaseVar [DP.DataPath i]
        | Local
        | Unknown 
    deriving (Eq,Ord,Generic,NFData)

instance (Show i) => Show (AccessPath i) where
    show (AccessPath b s) = (show b) ++ (concat $ (++ ".*") . tail . show <$> reverse s) ++ ")"
    show  Local           = "local"
    show  Unknown         = "?"


-- constructors for access paths

unknown :: AccessPath i
unknown = Unknown

parameter :: Int -> AccessPath i
parameter i = AccessPath (Parameter i) [DP.Root]

global :: IR.Tree -> AccessPath i
global g = AccessPath (Global g) [DP.Root]

local :: AccessPath i
local = Local



-- manipulation

append :: (Eq i) => AccessPath i -> DP.DataPath i -> AccessPath i
append Unknown    _ = Unknown
append Local      _ = Local
append _ DP.Invalid = Unknown

append (AccessPath b (d:ds)) p = case DP.append d p of
    DP.Invalid -> Unknown
    r@_        -> AccessPath b (r:ds)
 

deref :: AccessPath i -> AccessPath i
deref  Unknown          = Unknown
deref  Local            = Local
deref (AccessPath b ds) = AccessPath b (DP.Root:ds) 


extend :: AccessPath i -> AccessPath i -> AccessPath i
extend Unknown _ = Unknown
extend _ Unknown = Unknown
extend Local   _ = Local
extend _   Local = Local
extend (AccessPath v a) (AccessPath _ b) = AccessPath v (a ++ (tail b))
 
