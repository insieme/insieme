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

namespace insieme {
namespace core {
namespace lang {

	/**
	 * This extension should take care of compound ops (e.g. a += 1), where
	 * the left hand side has to be executed only once.
	 */
	class CompoundOpsExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		CompoundOpsExtension(core::NodeManager &manager) : core::lang::Extension(manager) {}

	public:

		// Import the reference module for ref-operations
		IMPORT_MODULE(ReferenceExtension)

		// Small macro that puts the given operator OP into the target inspire code
		// Note for frontend: Normally, in C, char types are implicitly casted to int by
		// clang - this is not the case for the LHS of compound assignments, because there
		// are no lvalue casts in C. To handle the different LHS and RHS types, we need
		// two different input vars 'a and 'b as well as back and forth num casting.
		#define INSPIRE_COMP_OP_CODE(OP) \
			"(left : ref<'a>, right : 'b) -> ref<'a> {                                       " \
			"    left = num_cast(num_cast(*left, type_lit('b)) " #OP " right, type_lit('a)); " \
			"    return left;                                                                " \
			"}                                                                               "

		LANG_EXT_DERIVED(CompAssignAdd,        INSPIRE_COMP_OP_CODE(+ ) )
		LANG_EXT_DERIVED(CompAssignSubtract,   INSPIRE_COMP_OP_CODE(- ) )
		LANG_EXT_DERIVED(CompAssignMultiply,   INSPIRE_COMP_OP_CODE(* ) )
		LANG_EXT_DERIVED(CompAssignDivide,     INSPIRE_COMP_OP_CODE(/ ) )
		LANG_EXT_DERIVED(CompAssignModulo,     INSPIRE_COMP_OP_CODE(% ) )
		LANG_EXT_DERIVED(CompAssignBitwiseAnd, INSPIRE_COMP_OP_CODE(& ) )
		LANG_EXT_DERIVED(CompAssignBitwiseOr,  INSPIRE_COMP_OP_CODE(| ) )
		LANG_EXT_DERIVED(CompAssignBitwiseXor, INSPIRE_COMP_OP_CODE(^ ) )
		LANG_EXT_DERIVED(CompAssignLeftShift,  INSPIRE_COMP_OP_CODE(<<) )
		LANG_EXT_DERIVED(CompAssignRightShift, INSPIRE_COMP_OP_CODE(>>) )

		#undef INSPIRE_COMP_OP_CODE

	};

	bool isCompoundAssignmentOperation(const core::NodePtr& node);

} // end namespace lang
} // end namespace core
} // end namespace insieme
