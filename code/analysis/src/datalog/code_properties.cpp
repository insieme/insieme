#include "insieme/analysis/datalog/code_properties.h"

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include "souffle/gen/polymorph_types_analysis.h"

namespace insieme {
namespace analysis {
namespace datalog {


	/**
	 * Determines whether the given type is a polymorph type.
	 */
	bool isPolymorph(const core::TypePtr& type, bool debug) {

		// instantiate the analysis
		souffle::Sf_polymorph_types_analysis analysis;

		// fill in facts
		int rootNodeID = framework::extractFacts(analysis, type);

		// add start node
		analysis.rel_rootNode.insert(rootNodeID);

		// print debug information
		if (debug) analysis.dumpInputs();

		// run analysis
		analysis.run();

		// print debug information
		if (debug) analysis.dumpOutputs();

		// read result
		auto& rel = analysis.rel_result;
		return rel.size() > 0;
	}


} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

