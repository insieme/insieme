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

{- | The data structure defined in this module is used to capture one analysis
   context.

It brings together the 'IR.Tree' of the program, the 'Solver.SolverState' and
an (optional) reference to an instance of the
@insieme::analysis::haskell::Context@ data structure in INSIEME.

 -}

module Insieme.Context (
    CContext,
    Context,

    -- * Constructors
    mkContext,
    mkDummyContext,

    -- * Queries
    getCContext,
    getTree,
    getSolverState,
) where

import Foreign.Ptr
import qualified Insieme.Inspire as IR
import qualified Insieme.Analysis.Solver as Solver

type CContext = Ptr ()

data Context = Context { getCContext    :: CContext,
                         getTree        :: IR.Tree,
                         getSolverState :: Solver.SolverState }

-- | Create a new 'Context', it will be initialized with an /empty/
-- 'Solver.SolverState'.
mkContext :: CContext -> IR.Tree -> Context
mkContext i c = Context i c Solver.initState

-- | Create a new 'Context' without a reference to a
-- @insieme::analysis::haskell::Context@ instance.
mkDummyContext :: IR.Tree -> Context
mkDummyContext = mkContext nullPtr
