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
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Entities.SymbolicFormula where

import Control.DeepSeq
import GHC.Generics (Generic)
import Insieme.Utils.ParseInt
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Utils.Arithmetic as Ar

--
-- * Arithemtic Symbol
--

data Symbol = Constant {getNode :: IR.Tree,
                        getAddr :: Addr.NodeAddress }
            | Variable {getNode :: IR.Tree,
                        getAddr :: Addr.NodeAddress }
  deriving (Generic, NFData)

instance Eq Symbol where
    x == y = (getNode x) == (getNode y)

instance Ord Symbol where
    compare x y = compare (getNode x) (getNode y)

instance Show Symbol where
    show (Constant (IR.Node IR.Literal  [_, IR.Node (IR.StringValue v) _]) _) = v
    show (Variable (IR.Node IR.Variable [_, IR.Node (IR.UIntValue   v) _]) _) = "v" ++ show v
    show _ = "???"

type SymbolicFormula = Ar.Formula CInt Symbol
