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

#include "insieme/core/ir_builder.h"

#include "insieme/analysis/cba_interface.h"

namespace insieme {
namespace analysis {
namespace dispatcher {

	/*
	 * Enumeration of the possible backends
	 */
	enum class Backend { DATALOG, HASKELL };


	/*
	 * A macro to create dispatches for the different backends.
	 * Needed because GTest can't generate parametrized tests if the parameter is a template argument.
	 */
	#define generate_dispatcher(FUNC, ...)                               \
	    switch(backend) {                                                \
	    case Backend::DATALOG: return analysis<DatalogEngine,getDefinitionPointAnalysis>(__VA_ARGS__);        \
	    case Backend::HASKELL: return analysis<DatalogEngine,getDefinitionPointAnalysis>(__VA_ARGS__);        \
	    default: assert_not_implemented() << "Backend not implemented!"; \
	    }                                                                \
	    return {};


	/*
	 * List of the dynamic dispatchers that should be available.
	 * This should provide functions of the form
	 *     dispatcher::dispatch_myFunctionName( Backend={DATALOG,HASKELL}, var1, var2, var3, ... )
	 * which can be used with GTest. Add more functions as needed.
	 */
	core::VariableAddress dispatch_getDefinitionPoint(Backend backend, const core::VariableAddress &var) {
//		generate_dispatcher(getDefinitionPoint, var)
		switch(backend) {
		case Backend::DATALOG: return analysis<DatalogEngine,getDefinitionPointAnalysis>(var);
		case Backend::HASKELL: return analysis<DatalogEngine,getDefinitionPointAnalysis>(var);
		default: assert_not_implemented() << "Backend not implemented!";
		}
		return {};
	}

	#define generate_function_pointer_dispatcher(FUNC, RET_VAL, ...)                  \
	    using FUNC##_signature = RET_VAL (*)(__VA_ARGS__);                            \
	    FUNC##_signature get_##FUNC(Backend backend) {                                \
	        switch(backend) {                                                         \
	        case Backend::DATALOG: return ::insieme::analysis::datalog::FUNC;         \
	        case Backend::HASKELL: return ::insieme::analysis::datalog::FUNC;         \
	        default: assert_not_implemented() << "Backend not implemented!";          \
	        }                                                                         \
	        return nullptr;                                                           \
	    }

	generate_function_pointer_dispatcher(areAlias, bool, const core::ExpressionAddress&, const core::ExpressionAddress&)
	generate_function_pointer_dispatcher(mayAlias, bool, const core::ExpressionAddress&, const core::ExpressionAddress&)
	generate_function_pointer_dispatcher(notAlias, bool, const core::ExpressionAddress&, const core::ExpressionAddress&)



	/*
	 * Undefining the macro to avoid namespace pollution
	 */
	#undef generate_dispatcher
	#undef generate_function_pointer_dispatcher

} // end namespace dispatcher
} // end namespace analysis
} // end namespace insieme
