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

#include <utility>

#include "insieme/analysis/common/integer_set.h"
#include "insieme/core/ir_address.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {

	// -----------------------------------------------------------------------------------------------------------


	/**
	 * A base class for all analysis engines.
	 */
	template<typename Ctxt>
	struct analysis_engine {
		using context_type = Ctxt;
	};


	// -----------------------------------------------------------------------------------------------------------

	// some internal constructs for creating the facade
	namespace detail {

		// a base class for defining the signature of analysis
		template<typename Res, typename ... Args>
		struct analysis_type {

			// a utility struct for adding backend information
			template<typename Backend>
			struct by {

				// the context type to be used by this backend
				using ctxt_type = typename Backend::context_type;

				// the signature of an implementing function
				using fun_type = Res(*)(ctxt_type&, const Args&...);

				// a utility struct for generating wrapper and dispatcher functions
				template<fun_type impl>
				struct with {

					// mark this analysis as being available
					enum { available = true };

					// run the analysis
					Res operator()(ctxt_type& ctxt, const Args& ... args) const {
						return (*impl)(ctxt, args...);
					}
				};

			};
		};

		/**
		 * Specializations of this struct link analysis facades to there
		 * implementations within different backends.
		 */
		template<typename Analysis, typename Backend>
		struct analysis_binding {
			enum { available = false };		// used to identify unmapped analysis
		};

	} // end namespace detail


	/**
	 * A macro to declare an analysis with an arbitrary number of parameters.
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 */
	#define declare_analysis( NAME, RESULT, ... ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT,__VA_ARGS__> {}; } \
		template<typename Backend, typename ... Args> RESULT NAME(typename Backend::context_type& ctxt, const Args& ... args) { \
			static_assert(detail::analysis_binding<detail::NAME##_Analysis,Backend>::available, "The analysis " #NAME " is not available for the selected backend!"); \
			return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(ctxt,args...); \
		} \
		template<typename Backend, typename ... Args> RESULT NAME(const Args& ... args) { \
			typename Backend::context_type ctxt; \
			return NAME<Backend>(ctxt, args... ); \
		}


	/**
	 * A macro to declare an analysis with one argument
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 * @param A1 the type of the first parameter
	 */
	#define declare_analysis_1( NAME, RESULT , A1 ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT,A1> {}; } \
		template<typename Backend> RESULT NAME(typename Backend::context_type& ctxt, const A1& a) { \
			static_assert(detail::analysis_binding<detail::NAME##_Analysis,Backend>::available, "The analysis " #NAME " is not available for the selected backend!"); \
			return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(ctxt,a); \
		} \
		template<typename Backend> RESULT NAME(const A1& a) { \
			typename Backend::context_type ctxt; \
			return NAME<Backend>(ctxt, a); \
		}

	/**
	 * A macro to declare an analysis with two arguments
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 * @param A1 the type of the first parameter
	 * @param A2 the type of the second parameter
	 */
	#define declare_analysis_2( NAME, RESULT , A1 , A2 ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT,A1,A2> {}; } \
		template<typename Backend> RESULT NAME(typename Backend::context_type& ctxt, const A1& a, const A2& b) { \
			static_assert(detail::analysis_binding<detail::NAME##_Analysis,Backend>::available, "The analysis " #NAME " is not available for the selected backend!"); \
			return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(ctxt,a,b); \
		} \
		template<typename Backend> RESULT NAME(const A1& a, const A2& b) { \
			typename Backend::context_type ctxt; \
			return NAME<Backend>(ctxt, a, b); \
		}


	/**
	 * A macro to be used by backend implementations to register an implementation of an analysis.
	 * @param BACKEND the backend identification token
	 * @param NAME the name of the analysis implemented, as declared by a macro above
	 * @param FUNCTION the function implementing the analysis
	 */
	#define register_analysis_implementation( BACKEND, NAME, FUNCTION ) \
		namespace detail { template<> struct analysis_binding<NAME##_Analysis,BACKEND> : public NAME##_Analysis::template by<BACKEND>::template with<FUNCTION> {}; }


} // end of namespace analysis
} // end of namespace insieme
