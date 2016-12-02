#include "insieme/analysis/cba/datalog/facts_to_files_extractor.h"

#include <iostream>
#include <sstream>

#include "insieme/analysis/cba/datalog/framework/file_extractor.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	bool extractPointerFactsToFiles(const TargetRelations &targets, const string &edCmds, const std::string &outputDir, bool debug, std::ostream &debugOut)
	{
		if (targets.size() == 0) {
			std::cerr << "Need at least one target node...not able to deduce root now. Aborting...." << std::endl;
			return -1;
		}

		std::map<string,std::set<const TargetRelationEntry*>> targetFacts;
		int ret;

		auto isTarget = [&](const std::string &relation, const core::NodeAddress &x) -> const TargetRelationEntry* {
			for (const auto &trg : targets.at(relation))
				if (trg == x) return &trg;
			return nullptr;
		};

		const auto &root = ((targets.begin())->second.begin())->getRootAddress();

		ret = framework::extractAddressFactsToFiles(outputDir, root, [&](const core::NodeAddress &curr, int id) {
			for (const auto &trgPair : targets) {
				const string &relation = trgPair.first;
				if (const auto *entry = isTarget(relation, curr)) {
					entry->setID(id);
					targetFacts[relation].insert(entry);
				}
			}
		}, debug, debugOut);

		if (ret == -1)
			return false;

		for (const auto &trgPair : targetFacts) {
			const string &relation = trgPair.first;
			const auto &entries = trgPair.second;

			// Build output lines from target relation entries
			std::set<std::string> values;
			for (const auto &e : entries) {
				std::stringstream ss;
				ss << e->getID() << "\t";
				for (const auto &info : e->getInfo())
					ss << info.second << "\t";
				ss << e->getNode();
				values.insert(ss.str());
			}

			ret = framework::addFactsManually(outputDir, relation, values, debug, debugOut);
			if (ret == -1)
				return false;
		}

		if (edCmds.size() > 0) {
			ret = framework::addFactsManually(outputDir, "__ED_CMDS", {edCmds}, debug, debugOut);
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
