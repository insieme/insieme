#pragma once

#include "insieme/analysis/cba/datalog/framework/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {
namespace framework {

	/**
	 * Checks for failure states in the given analysis. If failures are encountered, an exception will be thrown.
	 */
	void checkForFailures(souffle::Program& analysis);

} // end namespace framework
} // end namespace datalog
} //'end namespace cba
} // end namespace analysis
} // end namespace insieme
