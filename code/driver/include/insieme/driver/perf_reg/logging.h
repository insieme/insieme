#pragma once

#include <string>
#include <set>
#include <map>


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;

	class TestResultsLogger {
		using Test  = string;
		using Step  = string;
		using Key   = string;
		using Value = string;

		string id;
		set<Key> keys;
		map<Test, map<Step, map<Key,Value>>> results;

	public:
		void setID(const string &id);
		void addKey(const Key &key);
		bool addResult(const Test &test, const Step &step, const Key &key, const double &value);
		bool writeResultsToFile(const string &filename);
		bool writeResultsToStream(ostream &out);

		static TestResultsLogger &getInstance() {
			static TestResultsLogger instance;
			return instance;
		}

	private:
		TestResultsLogger() {}
		TestResultsLogger(const TestResultsLogger &) = delete;
		TestResultsLogger &operator=(const TestResultsLogger &) = delete;

		bool checkData();
		string printKeySet();
		void generateOutput(ostream &out);
	};

} // namespace perf_reg
} // namespace driver
} // namespace insieme
