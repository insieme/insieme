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

#include "insieme/driver/perf_reg/logging.h"

#include <ctime>
#include <fstream>
#include <sstream>

#include "insieme/utils/logging.h"

#include "boost/format.hpp"


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;
	using bf = boost::format;


	void TestResultsLogger::setID(const string& id)
	{
		if (this->id.size() > 0) {
			LOG(WARNING) << "Overwriting ID, which was previously set to " << this->id;
		}
		this->id = id;
	}

	void TestResultsLogger::addKey(const Key& key) {
		keys.insert(key);
	}

	bool TestResultsLogger::addResult(const Test& test, const Step& step, const Key& key, const double& value) {
		if (!keys.count(key)) {
			LOG(ERROR) << "Given key '" << key << "' is unknown!";
			return false;
		}

		if (results.count(test) &&
		    results.at(test).count(step) &&
		    results.at(test).at(step).count(key))
		{
			LOG(WARNING) << "Overwriting previous value for "
			             << test << " - " << step << " - " << key << "...";
		}

		results[test][step][key] = to_string(value);

		return true;
	}

	bool TestResultsLogger::writeResultsToFile(const string& filename) {
		ofstream file;
		file.open(filename, ios::out | ios::trunc);

		if (!file.is_open()) {
			LOG(ERROR) << "Could not open file '" << filename << "' for writing!";
			return false;
		}

		if (!checkData())
			return false;

		generateOutput(file);

		file.close();
		return true;
	}

	bool TestResultsLogger::writeResultsToStream(std::ostream& out) {
		if (!checkData())
			return false;
		generateOutput(out);
		return true;
	}

	bool TestResultsLogger::checkData() {
		for (const auto &testPair : results) {
			for (const auto &stepPair : testPair.second) {
				for (const auto &key : keys) {

					const auto &test = testPair.first;
					const auto &step = stepPair.first;

					if (!results.at(test).at(step).count(key)) {
						LOG(ERROR) << "Missing data: I have no " << key
						           << " in " << test << " - " << step << "!";
						return false;
					}
				}
			}
		}
		return true;
	}

	string TestResultsLogger::printKeySet() {
		bool first = true;
		stringstream out;
		for (const auto &key : keys) {
			if (!first) out << ";";
			else first = false;
			out << key;
		}
		return out.str();
	}

	void TestResultsLogger::generateOutput(std::ostream& out) {
		time_t ts = time(nullptr);
		string curr_time{ctime(&ts)};

		out << bf("$ Timestamp <%u>\n") % ts;
		out << bf("# %s") % curr_time;
		out << bf("$ Columns <Test;Step;%s>\n") % printKeySet();

		if (id.size() > 0)
			out << bf("$ ID <%s>\n") % id;

		for (const auto &testPair : results) {
			for (const auto &stepPair : testPair.second) {
				const auto &test = testPair.first;
				const auto &step = stepPair.first;

				out << bf("%s;%s") % test % step;
				for (const auto &key : keys)
					out << bf(";%s") % results.at(test).at(step).at(key);
				out << endl;
			}
		}
	}










} // namespace perf_reg
} // namespace driver
} // namespace insieme
