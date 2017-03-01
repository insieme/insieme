/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 */
#pragma once

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/pointer.h"

namespace insieme {
namespace core {
namespace lang {

	/**
	 * Extension providing memory management operations.
	 */
	class MemoryExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		MemoryExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		// this extension is based upon the symbols defined by the pointer module
		IMPORT_MODULE(PointerExtension);

		/**
		* Semantic translation of "malloc" (allocate an array of elements on the heap)
		*/
		LANG_EXT_DERIVED(MallocWrapper, R"((size : uint<8>) -> ptr<unit> {
			var uint<inf> si = size;
			return ptr_reinterpret(ptr_from_array(ref_new(type_lit(array<uint<1>,#si>))), type_lit(unit));
		})");

		/**
		* Semantic translation of "free" (frees pointer allocated on the heap)
		*/
		LANG_EXT_DERIVED(FreeWrapper, R"((trg : ptr<unit>) -> unit { ref_delete(ptr_to_ref(trg)); })");

	};

} // end namespace lang
} // end namespace core
} // end namespace insieme
