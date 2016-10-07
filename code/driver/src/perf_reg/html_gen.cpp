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

#include "insieme/driver/perf_reg/html_gen.h"

#include <ctime>
#include <fstream>

#include "boost/format.hpp"

#include "insieme/utils/logging.h"

#include "insieme/driver/perf_reg/structures.h"
#include "insieme/driver/perf_reg/options.h"


namespace insieme {
namespace driver {
namespace perf_reg {

	using namespace std;
	using bf = boost::format;


	template <typename T>
	string printDataPointVector(const vector<DataPoint> &vec, T member, bool asString = false)
	{
		auto &args = Options::getInstance();
		auto it = (vec.size() < args.plotLastX) ? vec.rbegin() : vec.rend() - args.plotLastX;

		stringstream ss;
		bool first = true;
		while (it != vec.rend()) {
			if (!first) ss << ", ";
			else first = false;
			if (asString) ss << "'" << (*it).*member << "'";
			else          ss << (*it).*member;
			++it;
		}
		return ss.str();
	}


	string getDivID() {
		static int i = 0;
		return to_string(i++);
	}


	double calcTotalAvg(const vector<DataPoint> &vec)
	{
		double sum = 0;
		for (const auto &dp : vec)
			sum += dp.val;
		return sum / vec.size();
	}


	void generateHTML(ostream& html, const map<string, string>& units)
	{
		auto &data = Data::getInstance();
		auto &args = Options::getInstance();

		time_t curr_time = time(nullptr);

		auto css = [&]() {
			return bf("<style type=\"text/css\">             \n"
			          "  h1 {                                \n"
			          "    border-bottom: 3px solid #333333; \n"
			          "  }                                   \n"
			          "                                      \n"
			          "  h2 {                                \n"
			          "    padding-top: 25px;                \n"
			          "    color: #660033;                   \n"
			          "    background-color: #EEDDDD;        \n"
			          "  }                                   \n"
			          "                                      \n"
			          "  h3 {                                \n"
			          "    margin-top: 10px;                 \n"
			          "    padding-top: 30px;                \n"
			          "    border-top: 2px solid #CCCCCC;    \n"
			          "  }                                   \n"
			          "</style>                              \n\n");
		};

		auto plotHeader = [&]() {
			return bf("<script src=\"plotly-latest.min.js\"></script>                                    \n"
			          "<script>                                                                          \n"
			          "  var ts2string = function(ts) {                                                  \n"
			          "    return Plotly.d3.time.format('%%Y-%%m-%%d %%H:%%M:%%S')(new Date(ts * 1000)); \n"
			          "  };                                                                              \n"
			          "</script>                                                                         \n");
		};

		auto header = [&]() {
			html << bf("<html>                                               \n"
			           "<head>                                               \n"
			           "  <title>Performance regression test results</title> \n"
			           "  %s                                                 \n"
			           "  %s                                                 \n"
			           "</head>                                              \n"
			           "<body>                                               \n\n"
			           "<h1>Performance regression report</h1>               \n\n"
			           "<p>                                                  \n"
			           "Created: <i>%s</i>                                   \n"
			           "</p>                                                 \n\n")
			        % plotHeader() % css() % ctime(&curr_time);
		};

		auto plotDefinition = [&](const string &key, const string &unit) {
			string xAxisTitle = (args.idAsXAxis) ? "Build Number" : "Date of measurement";

			html << bf("<script>                                      \n"
			           "  var plot_settings = {                       \n"
			           "    xaxis: { title : '%s' },                  \n"
			           "    yaxis: { title : '%s (%s)' }              \n"
			           "  };                                          \n"
			           "</script>                                     \n")
			        % xAxisTitle % key % unit;
		};

		auto footer = [&](){
			html << bf("</body> \n"
			           "</html> \n\n");
		};

		auto keyTitle = [&](const string &key, size_t count) {
			html << bf("<h2>Warnings for %s (%u total)</h2> \n") % key % count;
		};

		auto analysisDigest = [&](const AnalysisResult &ar, const string &unit) {
			return bf("Average of %.3f %s, now %.3f %s (%+.3f %s, %+.1f%%)")
			                % ar.avgValue % unit
			                % ar.currValue % unit
			                % ar.valueOverAvg % unit
			                % ar.percentOverAvg;
		};

		auto plotCode = [&](const vector<DataPoint> &vec, double avg) {
			auto &args = Options::getInstance();

			string id = getDivID();

			string xAxis, xAxisFormat, yAxis, additionalInfo, addInfoFormat;
			string avgXAxis;

			auto avgStartPoint = (vec.size() < args.plotLastX) ? vec.back() : vec.at(args.plotLastX - 1);

			if (args.idAsXAxis) {
				xAxis          = printDataPointVector(vec, &DataPoint::id);
				yAxis          = printDataPointVector(vec, &DataPoint::val);
				additionalInfo = printDataPointVector(vec, &DataPoint::ts);
				addInfoFormat  = ".map(ts2string)";
				avgXAxis       = (bf("%u, %u") % avgStartPoint.id % vec.front().id).str();
			} else {
				xAxis          = printDataPointVector(vec, &DataPoint::ts);
				yAxis          = printDataPointVector(vec, &DataPoint::val);
				additionalInfo = printDataPointVector(vec, &DataPoint::id, true);
				xAxisFormat    = ".map(ts2string)";
				avgXAxis       = (bf("%u, %u") % avgStartPoint.ts % vec.front().ts).str();
			}

			return bf("<div id=\"%s\"></div>                               \n"
			          "<script>                                            \n"
			          "  var trace = {                                     \n"
			          "    x: [%s]%s,                                      \n"
			          "    y: [%s],                                        \n"
			          "    name: 'History',                                \n"
			          "    type: 'scatter',                                \n"
			          "    mode: 'lines+markers',                          \n"
			          "    text: [%s]%s,                                   \n"
			          "  };                                                \n"
			          "  var avg = {                                       \n"
			          "    x: [%s]%s,                                      \n"
			          "    y: [%.3f, %.3f],                                \n"
			          "    name: 'Average',                                \n"
			          "    type: 'scatter'                                 \n"
			          "  };                                                \n"
			          "  Plotly.newPlot('%s', [trace,avg], plot_settings); \n"
			          "</script>                                           \n")
			                % id % xAxis % xAxisFormat % yAxis % additionalInfo % addInfoFormat
			                % avgXAxis % xAxisFormat % avg % avg
			                % id;
		};

		auto totalForKey = [&](const string &key) {
			html << bf("<p class=\"overall\">                 \n"
			           "<h3>Overall history for this key</h3> \n"
			           "  %s                                  \n"
			           "</p>                                  \n")
			        % plotCode(data.total.at(key), calcTotalAvg(data.total.at(key)));
		};

		auto analysisResult = [&](const AnalysisResult &ar, const string &unit, bool showGraph) {
			bf graph = showGraph
			           ? plotCode(ar.vec, ar.avgValue)
			           : bf("  <div><i>( Graph limit reached )</i></div> \n");

			html << bf("<p>                             \n"
			           "  <h3>Warning: %s / %s</h3>     \n"
			           "  <div>%s</div>                 \n"
			           "  %s                            \n"
			           "</p>                            \n\n")
			        % ar.test % ar.step % analysisDigest(ar, unit)
			        % graph;
		};

		auto noWarnings = [&]() {
			html << bf("<p>"
			           "  <h3>Good news!</h3>"
			           "  <div>No performance regressions could be found</div>"
			           "</p>");
		};


		header();

		if (data.result.size() == 0) {
			noWarnings();
		}

		for (const auto &totalPair : data.total) {
			const string &key = totalPair.first;
			size_t nrWarnings = data.result.count(key)
			                    ? data.result.at(key).size()
			                    : 0;
			const string &unit = units.at(key);

			keyTitle(key, nrWarnings);
			plotDefinition(key, unit);
			totalForKey(key);

			if (nrWarnings > 0) {
				int graphLimit = args.graphLimit;
				for (const auto &ar : data.result.at(key)) {
					analysisResult(ar, unit, (--graphLimit >= 0));
				}
			} else {
				noWarnings();
			}
		}

		footer();

	}

	bool writeHTML(const map<string, string>& units)
	{
		auto &args = Options::getInstance();

		if (args.outputFile.size() == 0) {
			generateHTML(cout, units);
			return true;
		}

		ofstream file;
		file.open(args.outputFile, ios::out | ios::trunc);

		if (!file.is_open()) {
			LOG(ERROR) << "Could not write to given output file " << args.outputFile << "!";
			return false;
		}

		generateHTML(file, units);

		file.close();

		return true;
	}


} // namespace perf_reg
} // namespace driver
} // namespace insieme
