/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#pragma once

#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/pointer.h"

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
		 * Allocates an array of "n" objects of type "typ" and calls the constructor for each of them
		 * (Intended to implement "new Bla[5]" semantics)
		 */
		LANG_EXT_DERIVED(ObjectArrayNew,
		"(typ : type<'a>, n : 'b, constructor : 'c::()) -> ptr<'a> {"
		"	auto ret = ref_new(type_lit(array<'a,#n>));"
		"	for(int<8> it = 0 ..  num_cast(n, type_lit(int<8>))) {"
		"		constructor(ref_array_elem(ret, it));"
		"	}"
		"	return ptr_from_array(ret);"
		"}")
	};

	// --------------------- Utilities ----------------------------

	/**
	 * Creates a C-style assignment operation
	 */
	core::ExpressionPtr buildCStyleAssignment(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs);

	/**
	 * Creates a C++-style assignment operation
	 */
	core::ExpressionPtr buildCxxStyleAssignment(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs);

	/**
	 * Creates a C-style comma operation
	 */
	core::ExpressionPtr buildCommaOperator(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs);

	/**
	 * Creates a an expression implementing C bool semantics
	 */
	core::ExpressionPtr buildBoolToInt(const core::ExpressionPtr& b);

	/**
	 * Allocates an array of "num" objects of type "objType" and calls the constructor "ctor" for each of them
	 */
	core::ExpressionPtr buildObjectArrayNew(const core::TypePtr& objType, const core::ExpressionPtr& num, const core::ExpressionPtr& ctor);

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
