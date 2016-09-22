#pragma once

#include <sstream>
#include <vector>
#include <fstream>
#include <regex>
#include <cmath>

#include <boost/lexical_cast.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace perf_reg {
namespace tools {

	using namespace std;

	string vec2string(vector<string> &vec);
	time_t str2time(const string &in);
	double str2double(const string &in);
	int str2int(const string &in);

	bool isDouble(const string &in);
	bool isInt(const string &in);

	double roundDouble(double in, double precision = 3);

	bool fileExists(const string &filename);
	bool allFilesExist(const vector<string> &filenames);

	bool extractSettingsValue(const string &in, const string &key, string &out);
	bool extractTimestamp(const string &in, time_t &out);

	bool quickWriteFile(const string filename, const string input);
	bool quickRemoveFile(const string filename);

} // namespace tools
} // namespace perf_reg
} // namespace driver
} // namespace insieme
