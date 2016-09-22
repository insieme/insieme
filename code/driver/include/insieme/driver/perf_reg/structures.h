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
