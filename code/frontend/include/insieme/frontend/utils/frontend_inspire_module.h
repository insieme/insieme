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

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/memory.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace frontend {
namespace utils {

	using namespace core;

	// --------------------- Extension ----------------------------

	/**
	 * A language extension which introduces some primitives necessary for C/C++ to INSPIRE translation.
	 */
	class FrontendInspireModule : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		FrontendInspireModule(core::NodeManager& manager);

	  public:

		// Import constructs from core's reference extension
		IMPORT_MODULE(core::lang::ReferenceExtension)
		IMPORT_MODULE(core::lang::ArrayExtension)
		IMPORT_MODULE(core::lang::PointerExtension)
		IMPORT_MODULE(core::lang::MemoryExtension)

		// -------------------- operators ---------------------------

		/**
		 * Implements a C style assignment (returning the assigned value)
		 */
		LANG_EXT_DERIVED(CStyleAssignment, "(lhs : ref<'a,f,'b>, rhs : 'a) -> 'a { lhs = rhs; return *lhs; }")

		/**
		 * Implements a C++ style assignment (returning the assigned value)
		 */
		LANG_EXT_DERIVED(CxxStyleAssignment, "(lhs : ref<'a,f,'b,'c>, rhs : 'a) -> ref<'a,f,'b,'c> { lhs = rhs; return lhs; }")

		/**
		 * Implements the C comma operator semantics
		 */
		LANG_EXT_DERIVED(CommaOperator, "(lhs : () => 'a, rhs : () => 'b) -> 'b { lhs(); return rhs(); }")

		/**
		 * Implements the C bool semantics
		 */
		LANG_EXT_DERIVED(BoolToInt, "(b : bool) -> int<4> { if(b) { return 1; } else { return 0; } }")

		/**
		 * Persistent ref-temp (not replaced by normal FE mechanisms)
		 */
		LANG_EXT_LITERAL(FERefTemp, "fe_ref_temp", "(type<'a>) -> ref<'a,f,f>")

		/**
		* Placement new for base types
		*/
		LANG_EXT_DERIVED(CxxPlacementNew, "(loc : ptr<unit,f,f>, val : 'a) -> ptr<'a,f,f> { ptr_to_ref(ptr_reinterpret(loc, type_lit('a))) = val; return ptr_reinterpret(loc, type_lit('a)); }")
	};

	// --------------------- Utilities ----------------------------

	core::ExpressionPtr buildCStyleAssignment(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs);
	core::ExpressionPtr buildCxxStyleAssignment(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs);

	core::ExpressionPtr buildCommaOperator(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs);

	core::ExpressionPtr buildBoolToInt(const core::ExpressionPtr& b);

	core::ExpressionPtr buildFERefTemp(const core::TypePtr& t);

	core::ExpressionPtr buildCxxPlacementNew(const core::ExpressionPtr& loc, const core::ExpressionPtr& init);

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
