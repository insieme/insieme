#pragma once

#include "insieme/analysis/common/integer_set.h"
#include "insieme/core/ir_address.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {

	// -----------------------------------------------------------------------------------------------------------

	// some internal constructs for creating the facade
	namespace detail {

		// a base class for defining the signature of analysis
		template<typename Res, typename ... Args>
		struct analysis_type {

			// the signature of an implementing function
			using fun_type = Res(*)(const Args&...);

			// a utility struct for generating wrapper and dispatcher functions
			template<fun_type impl>
			struct with {
				Res operator()(const Args& ... args) const {
					return (*impl)(args...);
				}
			};

		};

		/**
		 * Specializations of this struct link analysis facades to there
		 * implementations within different backends.
		 */
		template<typename Analysis, typename Backend>
		struct analysis_binding;

	} // end namespace detail


	/**
	 * A macro to declare an analysis with an arbitrary number of parameters.
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 */
	#define declare_analysis( NAME, RESULT, ... ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT,__VA_ARGS__> {}; } \
		template<typename Backend, typename ... Args> RESULT NAME(const Args& ... args) { return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(args...); }


	/**
	 * A macro to declare an analysis with no arguments
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 */
	#define declare_analysis_0( NAME, RESULT ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT> {}; } \
		template<typename Backend> RESULT NAME() { return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(); }

	/**
	 * A macro to declare an analysis with one argument
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 * @param A1 the type of the first parameter
	 */
	#define declare_analysis_1( NAME, RESULT , A1 ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT,A1> {}; } \
		template<typename Backend> RESULT NAME(const A1& a) { return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(a); }

	/**
	 * A macro to declare an analysis with two arguments
	 * @param NAME the name of the analysis
	 * @param RESULT the type of result produced by the analysis
	 * @param A1 the type of the first parameter
	 * @param A2 the type of the second parameter
	 */
	#define declare_analysis_2( NAME, RESULT , A1 , A2 ) \
		namespace detail { struct NAME##_Analysis : public analysis_type<RESULT,A1,A2> {}; } \
		template<typename Backend> RESULT NAME(const A1& a, const A2& b) { return detail::analysis_binding<detail::NAME##_Analysis,Backend>()(a,b); }


	/**
	 * A macro to be used by backend implementations to register an implementation of an analysis.
	 * @param BACKEND the backend identification token
	 * @param NAME the name of the analysis implemented, as declared by a macro above
	 * @param FUNCTION the function implementing the analysis
	 */
	#define register_analysis_implementation( BACKEND, NAME, FUNCTION ) \
		namespace detail { template<> struct analysis_binding<NAME##_Analysis,BACKEND> : public NAME##_Analysis::template with<FUNCTION> {}; }


} // end of namespace analysis
} // end of namespace insieme
