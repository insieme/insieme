#include "insieme/analysis/datalog/structs_and_pointers_analysis.h"

#include <iostream>

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include "souffle/gen/pointers.h"

namespace insieme {
namespace analysis {
namespace datalog {

	#ifdef ResultValueIsInteger
		#define convertValue std::to_string
	#else
		#define convertValue analysis.getSymbolTable().resolve
	#endif


	PointerResult runPointerAnalysis(const std::vector<core::ExpressionAddress>& targets) {
		souffle::Sf_pointers analysis;

		std::map<int,core::NodeAddress> targetIDs;

		auto isTarget = [&](const core::NodeAddress &x) {
			for (const auto &trg : targets)
				if (trg == x) return true;
			return false;
		};

		framework::extractAddressFacts(analysis, targets.at(0).getRootAddress(), [&](const core::NodeAddress &curr, int id) {
			if (isTarget(curr)) {
				targetIDs[id] = curr;
				analysis.rel_Targets.insert(id);
//				std::cout << "Target found: " << id << std::endl;
			}
		});

		analysis.run();

//		analysis.dumpOutputs();

		auto analysisResult = analysis.rel_Result;

		PointerResult results;
		for (const auto &res : analysisResult) {
			auto id      = res[0];
			auto defined = res[1];
			string value = convertValue(res[2]);

			core::NodeAddress addr = targetIDs[id];

			results[addr] = std::make_pair(defined,value);
		}

		return results;
	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
