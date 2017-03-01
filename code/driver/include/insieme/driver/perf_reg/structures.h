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
#pragma once

#include <ctime>
#include <vector>
#include <map>
#include <utility>

namespace insieme {
namespace driver {
namespace perf_reg {



	using namespace std;



	struct DataPoint {
		time_t ts;
		string id;
		double val;

		DataPoint(time_t ts, string id, double val)
		        : ts(ts), id(id), val(val) {}

		DataPoint(time_t ts, double val)
		        : ts(ts), id(), val(val) {}
	};



	struct AnalysisResult {
		const string &test, &step, &key;
		const vector<DataPoint> &vec;
		double avgValue, currValue;
		double valueOverAvg, percentOverAvg;

		AnalysisResult(const string &test,
		               const string &step,
		               const string &key,
		               const vector<DataPoint> &vec,
		               double avgValue,
		               double currValue)
		        : test(test),
		          step(step),
		          key(key),
		          vec(vec),
		          avgValue(avgValue),
		          currValue(currValue)
		{
			valueOverAvg = currValue - avgValue;
			percentOverAvg = ((currValue / avgValue) - 1.0) * 100.0;
		}

		bool operator <(const AnalysisResult &rhs) const {
			return percentOverAvg < rhs.percentOverAvg;
		}

		AnalysisResult(const AnalysisResult &other)
			: test(other.test)
			, step(other.test)
			, key (other.key)
			, vec (other.vec)
			, avgValue(other.avgValue)
			, currValue(other.currValue)
			, valueOverAvg(other.valueOverAvg)
			, percentOverAvg(other.percentOverAvg) { }

		AnalysisResult &operator =(const AnalysisResult &other) {
			return *new(this) AnalysisResult(other);
		}
	};



	struct Data {
		using TimestampIndex = vector<pair<time_t,string>>;
		using TSK = map<string,map<string,map<string,vector<DataPoint>>>>;
		using Total = map<string,vector<DataPoint>>;
		using AvgTSK = map<string,map<string,map<string,double>>>;

		using ResultSet = map<string,vector<AnalysisResult>>;

	public:
		TimestampIndex timestamps; // pair<ts,filename>
		TSK tsk;                   // test - step - key
		Total total;               // key - vector<DataPoint>
		AvgTSK avgTsk;             // test - step - key
		ResultSet result;          // key - vec<AnalysisResults>

		static Data &getInstance() {
			static Data instance;
			return instance;
		}

	private:
		Data() {}
		Data(Data const&) = delete;
		Data operator=(Data const&) = delete;
	};



} // namespace perf_reg
} // namespace driver
} // namespace insieme
