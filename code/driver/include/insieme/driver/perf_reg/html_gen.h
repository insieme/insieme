#pragma once

#include <ostream>
#include <map>
#include <string>

namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;

	bool writeHTML(const map<string,string> &units);


} // namespace perf_reg
} // namespace driver
} // namespace insieme
