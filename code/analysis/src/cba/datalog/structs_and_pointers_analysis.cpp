#include "insieme/analysis/cba/datalog/structs_and_pointers_analysis.h"

#include <iostream>

#include "insieme/analysis/cba/datalog/framework/souffle_extractor.h"

#include "souffle/gen/pointers.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	#ifdef ResultValueIsInteger
		#define convertValue std::to_string
	#else
		#define convertValue analysis.getSymbolTable().resolve
	#endif


	PointerResults runPointerAnalysis(Context& context, const std::set<core::ExpressionAddress>& targets) {
		PointerResults results;

		for (const auto &target : targets) {
			auto res = runPointerAnalysis(context, target);
			results[target] = res;
		}

		return results;

	}

	PointerResult runPointerAnalysis(Context &context, const core::ExpressionAddress &target) {
		const bool debug = false;

		// Instantiate analysis
		auto &analysis = context.getAnalysis<souffle::Sf_pointers>(target.getRootNode(), debug);

		// Get ID of target
		int targetExprID = context.getNodeID(target, debug);

		// Read result
		auto &resultRel = analysis.rel_Result;
		auto filtered = resultRel.template equalRange<0>({{targetExprID,0,0}});

		const auto &res = *(filtered.begin());
		const bool &defined = res[1];
		const auto &value = convertValue(res[2]);

		return std::make_pair(defined, value);
	}

	#undef convertValue

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
