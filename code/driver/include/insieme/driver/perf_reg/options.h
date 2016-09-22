#pragma once

#include <vector>
#include <string>

#include <boost/program_options.hpp>


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;
	namespace bpo = boost::program_options;

	struct Options {

		string outputFile;
		vector<string> inputFiles;
		vector<string> keys;
		unsigned precision;
		unsigned minDataPoints;
		unsigned avgOverLastX;
		int graphLimit;
		bool idAsXAxis;
		string failFile;

		bool parseCommandLine(int argc, char **argv);

		static Options &getInstance() {
			static Options instance;
			return instance;
		}

	private:
		Options() {}
		Options(Options const&) = delete;
		Options operator=(Options const&) = delete;

		bpo::variables_map vm;

		static void check_min_dp(unsigned val);
	};

} // namespace perf_reg
} // namespace driver
} // namespace insieme
