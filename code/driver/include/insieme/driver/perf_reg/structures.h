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

#pragma once

#include <ctime>
#include <vector>
#include <map>

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
