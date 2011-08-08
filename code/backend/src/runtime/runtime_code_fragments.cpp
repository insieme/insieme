/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/backend/runtime/runtime_code_fragments.h"

#include "insieme/backend/c_ast/c_ast_utils.h"

namespace insieme {
namespace backend {
namespace runtime {

	#define INIT_CONTEXT_NAME "insieme_init_context"
	#define CLEAN_CONTEXT_NAME "insieme_cleanup_context"
	#define TYPE_TABLE_NAME "g_insieme_type_table"
	#define IMPL_TABLE_NAME "g_insieme_impl_table"

	ContextHandlingFragment::ContextHandlingFragment(const Converter& converter) : converter(converter) {
		// add include to context definition and type and implementation table
		addInclude("irt_all_impls.h");
//		addDependency(converter->get)
	}

	const c_ast::IdentifierPtr ContextHandlingFragment::getInitFunctionName() {
		return converter.getCNodeManager()->create(INIT_CONTEXT_NAME);
	}

	const c_ast::IdentifierPtr ContextHandlingFragment::getCleanupFunctionName() {
		return converter.getCNodeManager()->create(CLEAN_CONTEXT_NAME);
	}

	std::ostream& ContextHandlingFragment::printTo(std::ostream& out) const {
		return out <<
				"void " INIT_CONTEXT_NAME "(irt_context* context) {\n"
				"    context->type_table = " TYPE_TABLE_NAME ";\n"
				"    context->impl_table = " IMPL_TABLE_NAME ";\n"
				"}\n;"
				"\n"
				"void " CLEAN_CONTEXT_NAME "(irt_context* context) {\n"
				"    // nothing to do \n"
				"}\n;";
	}


	const c_ast::ExpressionPtr TypeTable::getTypeTable() {
		return c_ast::ref(converter.getCNodeManager()->create(TYPE_TABLE_NAME));
	}

	std::ostream& TypeTable::printTo(std::ostream& out) const {
		return out;
	}


	const c_ast::ExpressionPtr ImplementationTable::getImplementationTable() {
		return c_ast::ref(converter.getCNodeManager()->create(IMPL_TABLE_NAME));
	}

	std::ostream& ImplementationTable::printTo(std::ostream& out) const {
		return out;
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme
