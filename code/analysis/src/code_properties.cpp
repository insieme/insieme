#include "insieme/analysis/datalog/code_properties.h"

#include "insieme/analysis/datalog/framework/analysis_base.h"

#include "souffle/gen/polymorph_types_analysis.h"


namespace insieme {
namespace analysis {
namespace datalog {

	namespace {

		class IsPolymorphAnalysis : public framework::AnalysisBase<souffle::Sf_polymorph_types_analysis> {

			const core::TypePtr& root;

		public:

			IsPolymorphAnalysis(const core::TypePtr& type) : root(type) {}


			bool get(bool debug) {
				// fill in facts
				int rootNodeID = this->extractFacts(root);

				// add start node
				this->rel_rootNode.insert(rootNodeID);


				// print debug information
				if (debug) this->dumpAll();

				// run analysis
				this->run();

				// print debug information
				if (debug) this->dumpAll();

				// read result
				auto& rel = this->rel_result;
				return rel.size() > 0;
			}

		};

	}



	/**
	 * Determines whether the given type is a polymorph type.
	 */
	bool isPolymorph(const core::TypePtr& type, bool debug) {
		return IsPolymorphAnalysis(type).get(debug);
	}


} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

