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


	/**
	 * OVERVIEW: MACROS TO CREATE DISPATCHERS FOR THE DIFFERENT BACKENDS.
	 *
	 * The first macro (generate_dispatcher) returns code to be placed in a function body,
	 * for a dispatcher function which actually performs the analysis and returns the result
	 * of that analysis.
	 *
	 * The second macro (generate_fptr_dispatcher) generates a full function which returns
	 * a function pointer to the analysis of the specified backend. The resulting function
	 * will be of the form 'get_ANALYSISFUNC(Backend::Whatever)'.
	 */



	/**
	 * Generate a function body (to be placed in a 'dispatcher_ANALYSISFUNC(Backend, params)' )
	 * to quickly generate a dynamic dispatcher function. This is useful when the analysis has no
	 * type information on which backend to use at compile-time.
	 *
	 * @param FUNC the function name of the analysis (e.g. getDefinitionPoint)
	 * @param __VA_ARGS__ the parameters for the analysis function (e.g. var1, var2, var3)
	 *
	 * @return The analysis result or a default-constructed 'return-type' object
	 */
	#define generate_dispatcher(FUNC, ...)                                                                            \
	    switch(backend) {                                                                                             \
	    case Backend::DATALOG: return (analysis<datalogEngine,getDefinitionPointAnalysis>()).operator()(__VA_ARGS__); \
	    case Backend::HASKELL: return (analysis<haskellEngine,getDefinitionPointAnalysis>()).operator()(__VA_ARGS__); \
	    default: assert_not_implemented() << "Backend not implemented!";                                              \
	    }                                                                                                             \
	    return {};



	/**
	 * Generate a new dispatcher function which returns a pointer to the analysis function of the
	 * specified backend. Again, this is useful when there is no backend type information available
	 * at compile-time.
	 *
	 * @param FUNC the function name of the analysis (e.g. areAlias)
	 * @param RET_VAL the return value of the analysis (e.g. bool)
	 * @param __VA_ARGS__ the parameters of the analysis function (e.g. const core::ExpressionAddress&, const core::....)
	 *
	 * @return A function pointer to the desired analysis or a default-constructed 'return-type' object
	 */
	#define generate_fptr_dispatcher(FUNC, RET_VAL, ...)                   \
	    using FUNC##_signature = RET_VAL (*)(__VA_ARGS__);                             \
	    FUNC##_signature get_##FUNC(Backend backend) {                                 \
	        switch(backend) {                                                          \
	        case Backend::DATALOG: return &(analysis<datalogEngine,FUNC##Analysis>()); \
	        case Backend::HASKELL: return &(analysis<haskellEngine,FUNC##Analysis>()); \
	        default: assert_not_implemented() << "Backend not implemented!";           \
	        }                                                                          \
	        return {};                                                                 \
	    }




	/*
	 * List of the dynamic dispatchers that should be available.
	 * This should provide functions of the form
	 *     dispatcher::dispatch_myFunctionName( Backend={DATALOG,HASKELL}, var1, var2, var3, ... )
	 * which can be used with GTest. Add more functions as needed.
	 */
	core::VariableAddress dispatch_getDefinitionPoint(Backend backend, const core::VariableAddress &var) {
		generate_dispatcher(getDefinitionPoint, var)
	}


	/*
	 * List of the dynamic function-pointer dispatchers that should be available.
	 * This should provide functions of the form
	 *     dispatcher::get_myFunctionName( Backend={DATALOG,HASKELL}, var1, var2, var3, ... )
	 * which can be used with GTest. Add more functions as needed.
	 */
	generate_fptr_dispatcher(areAlias, bool, const core::ExpressionAddress&, const core::ExpressionAddress&)
	generate_fptr_dispatcher(mayAlias, bool, const core::ExpressionAddress&, const core::ExpressionAddress&)
	generate_fptr_dispatcher(notAlias, bool, const core::ExpressionAddress&, const core::ExpressionAddress&)




	/*
	 * Undefining the macro to avoid namespace pollution
	 */
	#undef generate_dispatcher
	#undef generate_fptr_dispatcher

} // end namespace dispatcher
} // end namespace analysis
} // end namespace insieme
