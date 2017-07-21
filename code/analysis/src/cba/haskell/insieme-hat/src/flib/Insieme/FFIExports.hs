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

module Insieme.FFIExports where

import Insieme.Adapter

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Insieme.Analysis.SymbolicValue

import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Analysis.Alias as Alias
import qualified Insieme.Analysis.Arithmetic as Arith
import qualified Insieme.Analysis.Boolean as AnBoolean
import qualified Insieme.Analysis.Reference as Ref
import qualified Insieme.Analysis.Entities.FieldIndex as FieldIndex
import qualified Insieme.Analysis.Entities.SymbolicFormula as SymbolicFormula
import qualified Insieme.Analysis.Framework.PropertySpace.ComposedValue as ComposedValue
import qualified Insieme.Analysis.Solver as Solver
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR
import qualified Insieme.Inspire.BinaryParser as BinPar
import qualified Insieme.Inspire.BinaryDumper as BinDum
import qualified Insieme.Inspire.NodeAddress as Addr
import qualified Insieme.Inspire.Visit as Visit
import qualified Insieme.Utils.Arithmetic as Ar
import qualified Insieme.Utils.BoundSet as BSet

foreign export ccall "hat_freeStablePtr"
  freeStablePtr :: StablePtr a -> IO ()

foreign export ccall "hat_initialize_context"
  initializeContext :: Ctx.CContext -> CString -> CSize -> IO (StablePtr Ctx.Context)

foreign export ccall "hat_print_statistic"
  dumpStatistics :: StablePtr Ctx.Context -> IO ()

foreign export ccall "hat_dump_assignment"
  dumpAssignment :: StablePtr Ctx.Context -> IO ()

foreign export ccall "hat_mk_node_address"
  mkNodeAddress :: StablePtr Ctx.Context -> Ptr CSize -> CSize -> IO (StablePtr Addr.NodeAddress)

foreign export ccall "hat_node_path_length"
  nodePathLength :: StablePtr Addr.NodeAddress -> IO CSize

foreign export ccall "hat_node_path_poke"
  nodePathPoke :: StablePtr Addr.NodeAddress -> Ptr CSize -> IO ()

foreign export ccall "hat_find_declaration"
  findDecl :: StablePtr Addr.NodeAddress -> IO (StablePtr Addr.NodeAddress)

foreign export ccall "hat_check_boolean"
  checkBoolean :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt

foreign export ccall "hat_check_alias"
  checkAlias :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> StablePtr Addr.NodeAddress -> IO CInt

foreign export ccall "hat_arithmetic_value"
  arithValue :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (CSetPtr ArithmeticFormula)

foreign export ccall "hat_check_null"
  checkForNull :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt

foreign export ccall "hat_check_extern"
  checkForExtern :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt

foreign export ccall "hat_memory_locations"
  memoryLocations :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (CSetPtr MemoryLocation)

foreign export ccall "hat_test_formulaZero"
  testFormulaZero :: IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_test_formulaOne"
  testFormulaOne :: IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_test_formulaExample1"
  testFormulaExample1 :: StablePtr Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_test_formulaExample2"
  testFormulaExample2 :: StablePtr Addr.NodeAddress -> IO (CRepPtr ArithmeticFormula)

foreign export ccall "hat_hs_test_binary_dumper_mirror"
  testBinaryDumperMirror :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()

foreign export ccall "hat_hs_symbolic_values"
  hsSymbolicValues :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO (CSetPtr SymbolicValue)
