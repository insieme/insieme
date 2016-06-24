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

#include "insieme/utils/assert.h"
#include "insieme/core/ir_address.h"


/**
 * OVERVIEW: ADDING A NEW ANALYSIS TO THE CBA INTERFACE
 *
 * There are two macros:
 *
 * - add_cba_analysis specifies the name of the analysis function (ANALYSIS, e.g. getDefinitionPoint),
 *     the return value (RETURN, e.g. bool) and the parameters that the function takes (__VA_ARGS__).
 *
 * - add_cba_implementation links a backend to an analysis, using the parameters ENGINE (e.g. datalog, has to
 *     be lower-case!) and ANALYSIS (e.g. getDefinitionPoint, as given to the first macro).
 *
 * Each backend creates a 'struct backendEngine {}' (lower-case first character!) in it's own interface file.
 * Then it implements the desired analyses in it's own namespace. Finally, the analyses are made
 * available to the CBA interface using the add_cba_implementation macro, and the 'mybackend_interface.h'
 * file is included at the bottom of this file.
 *
 * Note: Unimplemented analyses have to be created and linked as well, if you plan to use the dynamic backend
 * dispatcher which is used in those tests that are common for all backends.
 */


/**
 * Declare an analysis using this macro
 *
 * @param ANALYSIS the function name (e.g. getDefinitionPoint)
 * @param RETURN the return value of the analysis (e.g. core::ExpressionAddress)
 * @param __VA_ARGS__ the parameters that the analysis function takes (e.g. core::ExpressionAddress)
 */
#define add_cba_analysis(ANALYSIS, RETURN, ...)                            \
    struct ANALYSIS##Analysis : public analysis_type<RETURN,__VA_ARGS__> {};


/**
 * Declare that some Backend implements some analysis
 *
 * @param ENGINE the backend which provides this analysis (e.g. datalog, written in lower-case like it's namespace!)
 * @param ANALYSIS the implemented analysis (e.g. areAlias, as declared in the first macro
 */
#define add_cba_implementation(ENGINE, ANALYSIS)                                   \
    template<>                                                                     \
    struct analysis<ENGINE##Engine,ANALYSIS##Analysis>                             \
                    : public ANALYSIS##Analysis::template with<ENGINE::ANALYSIS> {};


namespace insieme {
namespace analysis {


	/**
	 * The struct which is specialized to each declared implementation. In the end,
	 * there will be a struct typed as 'analysis<mybackend,myCBAfunc>', which has to
	 * be default-constructed and then called with
	 * - operator() to run the actual analysis, or
	 * - operator&  to get a pointer to the actual analysis implementation
	 */
	template<typename Framework, typename Analysis>
	struct analysis;


	/**
	 * The static analysis dispatcher. By using a combination of template magic and the
	 * macros defined above, this struct provides the actual implementation of the 'analysis' struct.
	 */
	template<typename Res, typename ... Args>
	struct analysis_type {
		using fun_type = Res(*)(const Args&...);

		template<fun_type f>
		struct with {
			Res operator()(const Args& ... args) const {
				return (*f)(args...);
			}
			fun_type operator&() const {
				return f;
			}
		};
	};


	/**
	 * A list of the available analyses.
	 */
	add_cba_analysis(areAlias, bool, core::ExpressionAddress, core::ExpressionAddress)
	add_cba_analysis(mayAlias, bool, core::ExpressionAddress, core::ExpressionAddress)
	add_cba_analysis(notAlias, bool, core::ExpressionAddress, core::ExpressionAddress)
	add_cba_analysis(getDefinitionPoint, core::VariableAddress, core::VariableAddress);


} // end namespace analysis
} // end namespace insieme


/**
 * Inclusion of the different backends. Each backend has its own interface file,
 * where it declares which analyses it implements.
 */
#include "insieme/analysis/datalog_interface.h"
#include "insieme/analysis/haskell_interface.h"


/**
 * Done. Avoid namespace pollution
 */
#undef add_cba_analysis
#undef add_cba_implementation
