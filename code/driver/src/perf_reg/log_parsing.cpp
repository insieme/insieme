#include "insieme/driver/perf_reg/log_parsing.h"

#include <ctime>
#include <fstream>
#include <algorithm>
#include <regex>
#include <set>

#include "boost/algorithm/string.hpp"

#include "insieme/driver/perf_reg/options.h"
#include "insieme/driver/perf_reg/structures.h"
#include "insieme/driver/perf_reg/toolbox.h"


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;


	bool scanTimestamps()
	{
		auto &args = Options::getInstance();
		auto &data = Data::getInstance();

		for (const auto &filename : args.inputFiles) {
			time_t ts;
			string line;
			ifstream file(filename);

			if (!file.is_open())
				return false;

			if (!getline(file, line)) {
				file.close();
				return false;
			}

			if (!tools::extractTimestamp(line, ts)) {
				file.close();
				return false;
			}

			auto entry = make_pair(ts, filename);
			data.timestamps.push_back(entry);
			file.close();
		}

		// Sort newest first
		sort(data.timestamps.rbegin(), data.timestamps.rend());

		return true;
	}

	namespace detail {

		struct State {
			bool foundColumnFormat = false;
			map<string,size_t> columns;

			bool foundID = false;
			string id;

			bool beganParsingData = false;

			static bool idIsUnique(const string &id) {
				static set<string> allIDs;
				if (allIDs.count(id))
					return false;
				allIDs.insert(id);
				return true;
			}
		};

		bool parseSettingsLine(const string &line, State &st)
		{
			auto &args = Options::getInstance();

			string buf;

			// Check if we can still parse settings
			if (st.beganParsingData) {
				LOG(ERROR) << "Too late for settings!";
				return false;
			}

			// Columns: Know what column contains what key
			if (tools::extractSettingsValue(line, "Columns", buf)) {
				st.foundColumnFormat = true;
				vector<string> splitted;

				// Split the line
				boost::split(splitted, buf, boost::is_any_of(":;,"));

				// Check if first two keys are test and step
				if (splitted.at(0) != "Test" || splitted.at(1) != "Step")
					return false;

				// Add key-to-colNr index
				bool addAllKeys = args.keys.size() == 0 ? true : false;

				for (size_t i = 0; i < splitted.size(); ++i) {
					const string &keyname = splitted.at(i);

					st.columns[keyname] = i;

					// If no keys to check were given, check for all
					if (addAllKeys && i > 1)
						args.keys.push_back(keyname);

				}

				// Make sure we should not index a key that does not exist
				for (const auto &key : args.keys) {
					if (st.columns.count(key) == 0) {
						LOG(ERROR) << "Should check for " << key << ", but "
						           << "it does not exist according to the column format "
						           << "specified in the file, which is " << buf << "!";
						return false;
					}
				}

			// Check if there is an ID which can be displayed in addition to the timestamp
			} else if (tools::extractSettingsValue(line, "ID", buf)) {
				if (!State::idIsUnique(buf)) {
					LOG(ERROR) << "Found id " << buf << " more than once, but it should be unique!";
					return false;
				}
				st.foundID = true;
				st.id = buf;

			// The timestamp line, but it has already been taken care of it
			} else if (tools::extractSettingsValue(line, "Timestamp", buf)) {
				void();

			// Encountered unknown option!
			} else {
				LOG(ERROR) << "Found unknown setting in line " << line << "!";
				return false;
			}

			return true;
		}

	}

	bool loadLogfiles()
	{
		auto &args = Options::getInstance();
		auto &data = Data::getInstance();

		bool newestFile = true;

		for (const auto &tsPair : data.timestamps) {
			string line;

			detail::State st;

			const string &filename = tsPair.second;
			const time_t &ts = tsPair.first;

			ifstream file(filename);

			if (!file.is_open()) {
				LOG(ERROR) << "Unable to open file " << filename << "!";
				return false;
			}

			while (getline(file, line)) {
				boost::trim(line);

				// Jump over empty lines
				if (line.size() == 0)
					continue;

				// Ignore comments
				if (line.at(0) == '#')
					continue;

				// Handle settings
				if (line.at(0) == '$') {
					if (!detail::parseSettingsLine(line, st)) {
						LOG(ERROR) << filename << ": Unable to parse settings (" << line << ")";
						file.close();
						return false;
					}
					continue;
				}

				// Mark that we begin parsing data -- too late for further settings!
				st.beganParsingData = true;

				// Parse log data (only possible if column format is known)
				if (!st.foundColumnFormat) {
					LOG(ERROR) << filename << ": Column format is missing";
					file.close();
					return false;
				}

				vector<string> values;
				boost::split(values, line, boost::is_any_of(":;,"));

				if (values.size() != st.columns.size()) {
					LOG(ERROR) << filename << ": Column format and number of given values mismatch";
					file.close();
					return false;
				}

				if (!st.foundID) {
					st.id = filename;
				}

				for (const auto &key : args.keys) {
					size_t idx = st.columns.at(key);

					if (!tools::isDouble(values[idx])) {
						LOG(ERROR) << filename << ": Given value for " << key << " is not a double";
						file.close();
						return false;
					}

					const string &test = values.at(0);
					const string &step = values.at(1);
					double val = tools::str2double(values[idx]);

					val = tools::roundDouble(val, args.precision);

					if (newestFile) {
						data.tsk[test][step][key].emplace_back(ts, st.id, val);
					} else if (data.tsk.count(test) &&
					           data.tsk.at(test).count(step) &&
					           data.tsk.at(test).at(step).count(key)) {
						data.tsk[test][step][key].emplace_back(ts, st.id, val);
					} else {
						LOG(WARNING) << filename << ": Ignoring " << test << "/" << step << " (not in newest log)";
					}

					if (data.total.count(key) &&
					    data.total.at(key).back().ts == ts)
					{
						data.total[key].back().val += val;
					} else {
						data.total[key].emplace_back(ts, st.id, val);
					}
				}

			}

			if (newestFile) newestFile = false;
		}

		return true;
	}


} // namespace perf_reg
} // namespace driver
} // namespace insieme
