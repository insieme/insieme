#include "insieme/analysis/cba/datalog/facts_to_files_extractor.h"

#include <iostream>

#include "insieme/analysis/cba/datalog/framework/file_extractor.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	bool extractPointerFactsToFiles(const TargetRelations &targets, const string &edCmds, const std::string &outputDir) {
		const bool debug = true;

		std::map<string,std::set<std::string>> targetFacts;
		int ret;

		auto isTarget = [&](const std::string &relation, const core::NodeAddress &x) {
			for (const auto &trg : targets.at(relation))
				if (trg == x) return true;
			return false;
		};

		const auto &root = ((targets.begin())->second.begin())->getRootAddress();

		ret = framework::extractAddressFactsToFiles(outputDir, root, [&](const core::NodeAddress &curr, int id) {
			for (const auto &trgPair : targets) {
				const string &relation = trgPair.first;
				if (isTarget(relation, curr)) {
					targetFacts[relation].insert(std::to_string(id));
				}
			}
		}, debug);

		if (ret == -1)
			return false;

		for (const auto &trgPair : targetFacts) {
			const string &relation = trgPair.first;
			const auto &values = trgPair.second;
			ret = framework::addFactsManually(outputDir, relation, values, debug);
			if (ret == -1)
				return false;
		}

		if (edCmds.size() > 0) {
			ret = framework::addFactsManually(outputDir, "__ED_CMDS", {edCmds});
			if (ret == -1)
				return false;
		}

		return true;
	}

	#undef convertValue

} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme
