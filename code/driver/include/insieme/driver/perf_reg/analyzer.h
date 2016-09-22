#pragma once

#include <functional>
#include <map>
#include <vector>

#include "insieme/driver/perf_reg/structures.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;

	class Analyzer {
	public:
		struct ArgType {
			const vector<DataPoint> &vec;
			const double &avg;
		};

	private:
		using analysisFunc = function<bool(ArgType)>;

		map<string,analysisFunc> funcs;
		analysisFunc defaultFunc = nullptr;

	public:
		void setDefault(analysisFunc func) {
			defaultFunc = func;
		}

		void mapFunction(const string &key, analysisFunc func) {
			funcs[key] = func;
		}

		analysisFunc getFunction(const string &key) {
			if (funcs.count(key))
				return funcs.at(key);

			if (!defaultFunc)
				return [](ArgType in) {
					(void) in;
					LOG(ERROR) << "Default not implemented!";
					return true;
				};

			return defaultFunc;
		}

		static Analyzer &getInstance() {
			static Analyzer instance;
			return instance;
		}

	private:
		Analyzer() {}
		Analyzer(Analyzer const&) = delete;
		Analyzer operator=(Analyzer const&) = delete;
	};

} // namespace perf_reg
} // namespace driver
} // namespace insieme
