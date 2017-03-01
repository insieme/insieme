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
#include "insieme/driver/perf_reg/data_processing.h"

#include <vector>
#include <map>
#include <string>
#include <algorithm>

#include "insieme/driver/perf_reg/options.h"
#include "insieme/driver/perf_reg/structures.h"
#include "insieme/driver/perf_reg/analyzer.h"


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;

	namespace detail {

		bool preprocessDataVec(double &avg, vector<DataPoint> &vec)
		{
			auto &args = Options::getInstance();

			// Check if data set is large enough
			if (vec.size() < args.minDataPoints)
				return false;

			// Calculate avg. Data is sorted from newest to oldest
			auto dp = vec.begin() + 1; // skip the newest value
			unsigned stepLimit = args.avgOverLastX;

			double sum = 0;
			while (stepLimit && dp != vec.end()) {
				sum += dp->val;
				--stepLimit;
				++dp;
			}

			double count = static_cast<double>(args.avgOverLastX - stepLimit);
			avg = sum / count;

			return true;
		}

	}


	bool preprocessData()
	{
		auto &data = Data::getInstance();

		for (auto &testPair : data.tsk) {
			const auto &test = testPair.first;
			auto &steps = testPair.second;

			for (auto &stepPair : steps) {
				const auto &step = stepPair.first;
				auto &keys = stepPair.second;

				auto it = keys.begin();
				while (it != keys.end()) {
					const string &key = it->first;
					vector<DataPoint> &value = it->second;

					double &avg = data.avgTsk[test][step][key];

					if (!detail::preprocessDataVec(avg, value))
						it = keys.erase(it);
					else
						++it;
				}
			}
		}

		return true;
	}


	void performanceRegressionCheck()
	{
		auto &data = Data::getInstance();
		auto &analyzer = Analyzer::getInstance();

		for (const auto &testPair : data.tsk) {
			const auto &test = testPair.first;
			const auto &steps = testPair.second;

			for (const auto &stepPair : steps) {
				const auto &step = stepPair.first;
				const auto &keys = stepPair.second;

				for (const auto &keyPair : keys) {

					const string &key = keyPair.first;
					const vector<DataPoint> &values = keyPair.second;

					double avg = data.avgTsk[test][step][key];
					double curr = values.at(0).val;

					auto func = analyzer.getFunction(key);

					Analyzer::ArgType funArgs = {values, avg};

					if (!func(funArgs)) {
						AnalysisResult res(test, step, key, values, avg, curr);
						data.result[key].push_back(res);
					}
				}
			}
		}

		// Sort findings by severity
		for (auto &vecPair : data.result) {
			auto &vec = vecPair.second;
			sort(vec.rbegin(), vec.rend());
		}
	}

} // namespace perf_reg
} // namespace driver
} // namespace insieme
