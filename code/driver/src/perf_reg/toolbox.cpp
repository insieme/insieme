/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#include "insieme/driver/perf_reg/toolbox.h"

#include <cmath>
#include <cstdio>

namespace insieme {
namespace driver {
namespace perf_reg {
namespace tools {

	template <typename T>
	string vecT2string(const vector<T> vec) {
		bool first = true;
		stringstream ss;

		for (const auto &elem : vec) {
			if (!first) ss << ", ";
			else first = false;
			ss << elem;
		}

		return ss.str();
	}


	string vec2string(vector<string>& vec) {
		return vecT2string(vec);
	}

	time_t str2time(const std::__cxx11::string& in) {
		try {
			return boost::lexical_cast<time_t>(in);
		} catch (boost::bad_lexical_cast &) {
			LOG(WARNING) << "Given string '" << in
			             << "' could not be casted to time_t!";
		}
		return 0L;
	}

	double str2double(const string& in) {
		return boost::lexical_cast<double>(in);
	}

	int str2int(const string& in) {
		return boost::lexical_cast<int>(in);
	}

	template <typename T>
	bool isNumber(const string &in) {
		try {
			boost::lexical_cast<T>(in);
		} catch (boost::bad_lexical_cast &) {
			return false;
		}
		return true;
	}

	bool isDouble(const string& in) {
		return isNumber<double>(in);
	}

	bool isInt(const string& in) {
		return isNumber<int>(in);
	}

	bool fileExists(const string& filename) {
		ifstream file(filename);
		return file.good();
	}

	bool allFilesExist(const vector<string>& filenames) {
		for (const auto &filename : filenames)
			if (!fileExists(filename)) return false;
		return true;
	}

	bool extractSettingsValue(const string& in, const string& key, string& out) {
		regex expr{"\\$ " + key + " <([^>]*)>"};
		smatch what;

		if (!regex_match(in, what, expr) || what.size() != 2)
			return false;

		out = what[1].str();
		return true;
	}

	bool extractTimestamp(const string& in, time_t& out) {
		string str;

		if (extractSettingsValue(in, "Timestamp", str)) {
			out = str2time(str);
			if (out) return true;
		}

		LOG(ERROR) << "Could not parse timestamp in line: " << in;
		return false;
	}

	double roundDouble(double in, double precision) {
		precision = pow(10.0, precision);
		return round((in * precision)) / precision;
	}

	bool quickWriteFile(const string filename, const string input)
	{
		ofstream file;
		file.open(filename);

		if (!file.is_open()) {
			LOG(ERROR) << "Could not open " << filename << " for writing";
			return false;
		}

		file << input;

		file.close();
		return true;
	}

	bool quickRemoveFile(const string filename)
	{
		return remove(filename.c_str());
	}

} // namespace tools
} // namespace perf_reg
} // namespace driver
} // namespace insieme
