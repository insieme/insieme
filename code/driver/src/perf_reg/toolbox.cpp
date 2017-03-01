/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include "insieme/driver/perf_reg/toolbox.h"

#include <cmath>
#include <cstdio>

using namespace std;

namespace insieme {
namespace driver {
namespace perf_reg {
namespace tools {

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
			try {
				out = boost::lexical_cast<time_t>(str);
				return true;
			} catch (boost::bad_lexical_cast &) {
				LOG(WARNING) << "Given string '" << str << "' could not be casted to time_t!";
			}
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
