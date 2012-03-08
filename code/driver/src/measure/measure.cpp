/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/driver/measure/measure.h"

#include <set>
#include <map>

#include <boost/filesystem.hpp>

#include "insieme/driver/driver_config.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace driver {
namespace measure {

	// -- metrics --

	namespace {

		std::map<string, MetricPtr> metric_register;

		MetricPtr registerMetric(const Metric& metric) {
			metric_register[metric.getName()] = &metric;
			return &metric;
		}


		// --- units to be used within the metrics ---

		auto ns = makeUnitPtr(nano * s);
		auto kb = makeUnitPtr(kilo * byte);
		auto wh = makeUnitPtr(Prefix(3600) * kg * (m^2) * (s^-2));		// = 1 Wh (?)
		auto cycle = makeUnitPtr(Unit("cycle"));
		auto unit = makeUnitPtr(Unit());

		// --- aggregation functions to be used ---

		Quantity none(const Measurements& data, MetricPtr metric, region_id region) {
			assert(false && "Extraction on region base not supported!");
			return Quantity::invalid(metric->getUnit());
		}

		Quantity sumOf(const UnitPtr& unit, const vector<Quantity>& list) {
			Quantity res(0, unit);
			for_each(list, [&](const Quantity& cur) {
				res += cur;
			});
			return res;
		}

		Quantity avgOf(const UnitPtr& unit, const vector<Quantity>& list) {

			// check whether there is something
			if (list.empty()) {
				return Quantity::invalid(unit);
			}

			// compute average
			Quantity res(0, unit);
			for_each(list, [&](const Quantity& cur) {
				res += cur;
			});
			return res / Quantity(list.size());
		}

		vector<Quantity> diffOf(const Measurements& data, region_id region, const MetricPtr& a, const MetricPtr& b) {
			vector<Quantity> dataA = data.getAll(region, a);
			vector<Quantity> dataB = data.getAll(region, b);
			vector<Quantity> res;
			assert(dataA.size() == dataB.size() && "Expecting same sequence of values for given metrics!");
			for(std::size_t i=0; i<dataA.size(); i++) {
				res.push_back(dataA[i] - dataB[i]);
			}
			return res;
		}

	}


	// -- Create pre-defined metrics ---

	// define metrics using the def file
	#define METRIC(LITERAL, NAME, UNIT, FUN) \
		Metric _##LITERAL(NAME, UNIT, FUN); \
		const MetricPtr Metric::LITERAL = registerMetric(_##LITERAL); \

	#define OP(EXPR) \
		[](const Measurements& data, MetricPtr metric, region_id region)->Quantity { \
			const auto& unit = metric->getUnit(); \
			auto sum = [&](const vector<Quantity>& list)->Quantity { return sumOf(unit, list); }; \
			auto avg = [&](const vector<Quantity>& list)->Quantity { return avgOf(unit, list); }; \
			auto diff = [&](const MetricPtr& a, const MetricPtr& b)->vector<Quantity> { return diffOf(data, region, a, b); }; \
			return EXPR; \
		}

	#include "insieme/driver/measure/metrics.def"

	#undef OP
	#undef METRIC

	const MetricPtr Metric::getForName(const string& name) {
		auto pos = metric_register.find(name);
		if (pos == metric_register.end()) {
			return MetricPtr();
		}
		return pos->second;
	}


	// -- measurements --


	Quantity measure(const core::StatementAddress& stmt, const MetricPtr& metric, const ExecutorPtr& executor) {

		// pack given stmt pointer into region map
		std::map<core::StatementAddress, region_id> regions;
		regions[stmt] = 0;

		// measure region and return result
		MetricPtr m = metric;
		return measure(regions, toVector(m), executor)[0][0];
	}


	namespace {

		/**
		 * This function is simply wrapping the given statement into instrumented start / end
		 * region calls.
		 *
		 * @param stmt the statement to be wrapped
		 * @param id the ID to be used within the wrapping region boundaries.
		 * @return the instrumented replacement for the given statement
		 */
		core::StatementPtr instrument(const core::StatementPtr& stmt, region_id id) {

			// instrument region
			auto& manager = stmt->getNodeManager();
			core::IRBuilder build(manager);

			// obtain references to primitives
			const auto& unit = manager.getLangBasic().getUnit();
			auto& rtExt = manager.getLangExtension<insieme::backend::runtime::Extensions>();

			// build instrumented code section
			auto regionID = build.intLit(id);
			auto region_inst_start_call = build.callExpr(unit, rtExt.instrumentationRegionStart, regionID);
			auto region_inst_end_call = build.callExpr(unit, rtExt.instrumentationRegionEnd, regionID);
			return build.compoundStmt(region_inst_start_call, stmt, region_inst_end_call);
		}

		/**
		 * Wraps the given code fragment into a program - to be processed by the backend.
		 */
		core::ProgramPtr wrapIntoProgram(const core::NodePtr& ptr) {

			// check whether it is already a program
			if (ptr->getNodeType() == core::NT_Program) {
				return ptr.as<core::ProgramPtr>();
			}

			auto& mgr = ptr->getNodeManager();
			core::IRBuilder builder(mgr);

			// get code fragment as a stmt
			core::StatementPtr stmt = ptr.as<core::StatementPtr>();

			// build entry point
			core::NodePtr main = builder.lambdaExpr(stmt, core::VariableList());

			// build enclosing program
			return builder.program(toVector(main));
		}

	}


	std::map<region_id, vector<Quantity>> measure(
			const std::map<core::StatementAddress, region_id>& regions,
			const vector<MetricPtr>& metrices, const ExecutorPtr& executor) {

		// fast exit if no regions are specified
		if (regions.empty()) {
			return std::map<region_id, vector<Quantity>>();
		}


		core::NodeManager& manager = regions.begin()->first->getNodeManager();

		// instrument individual regions
		std::set<region_id> regionIDs;
		std::map<core::NodeAddress, core::NodePtr> instrumented;
		for_each(regions, [&](const pair<core::StatementAddress, region_id>& cur) {
			instrumented[cur.first] = instrument(cur.first, cur.second);
			regionIDs.insert(cur.second);
		});

		// create resulting program
		core::ProgramPtr program = wrapIntoProgram(core::transform::replaceAll(manager, instrumented));


		// create backend code
		auto backend = insieme::backend::runtime::RuntimeBackend::getDefault();
		auto targetCode = backend->convert(program);

		// compile target code
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-O3");
		compiler.addFlag("-I " SRC_DIR "../../runtime/include -I " PAPI_HOME "/include  -L " PAPI_HOME "/lib/");
		compiler.addFlag("-D_XOPEN_SOURCE=700 -D_GNU_SOURCE -DIRT_ENABLE_REGION_INSTRUMENTATION");
		compiler.addFlag("-ldl -lrt -lpthread -lm -Wl,-Bstatic -lpapi -Wl,-Bdynamic");

		string binFile = utils::compiler::compileToBinary(*targetCode, compiler);
		if (binFile.empty()) {
			throw MeasureException("Unable to compiling executable for measurement!");
		}

		// run code
		int ret = executor->run(binFile);
		if (ret != 0) {
			throw MeasureException("Unable to run executable for measurements!");
		}

		// delete binary
		if (boost::filesystem::exists(binFile)) {
			boost::filesystem::remove(binFile);
		}

		// load data
		Measurements data = loadResults(".");

		// delete local files
//		if (ret==0) system("rm worker_*");

		// TODO:
		// - instrument regions
		// - limit execution !!!
		// - create binary
		// - run binary
		// - collect data
		// - delete files


		// collect requested information
		std::map<region_id, vector<Quantity>> res;
		for_each(regionIDs, [&](const region_id& region) {
			for_each(metrices, [&](const MetricPtr& metric) {
				// use extractor of metric to collect values
				res[region].push_back(metric->extract(data, region));
			});
		});

		// return result
		return res;
	}



	void Measurements::add(worker_id worker, region_id region, MetricPtr metric, const Quantity& value) {
		store[worker][region][metric].push_back(value);	// just add value
	}

	const vector<Quantity>& Measurements::getAll(worker_id worker, region_id region, MetricPtr metric) const {
		// the value returned in case there is no such entry
		static const vector<Quantity> empty = vector<Quantity>();

		// work down step by step

		// start with worker ..
		auto pos = store.find(worker);
		if (pos == store.end()) {
			return empty;
		}

		// .. continue with region ..
		auto pos2 = pos->second.find(region);
		if (pos2 == pos->second.end()) {
			return empty;
		}

		// .. and conclude with the metric
		auto pos3 = pos2->second.find(metric);
		if (pos3 == pos2->second.end()) {
			return empty;
		}
		return pos3->second;
	}

	vector<Quantity> Measurements::getAll(region_id region, MetricPtr metric) const {
		vector<Quantity> res;
		for_each(store, [&](const DataStore::value_type& value) {
			for_each(value.second, [&](const pair<region_id, std::map<MetricPtr, vector<Quantity>>>& cur) {
				if (cur.first == region) {
					auto pos = cur.second.find(metric);
					if (pos != cur.second.end()) {
						const vector<Quantity>& list = pos->second;
						res.insert(res.end(), list.begin(), list.end());
					}
				}
			});
		});
		return res;
	}


	namespace {

		vector<string> readLine(std::ifstream& in) {
			typedef boost::tokenizer< boost::escaped_list_separator<char> > Tokenizer;

			string line;
			std::getline(in, line);
			Tokenizer tok(line);
			return vector<string>(tok.begin(), tok.end());
		}

		void loadFile(const boost::filesystem::path& file, worker_id id, Measurements& res) {

			// open file
			std::ifstream in(file.string());
			if(!in.is_open()) return;

			// ---- Header ----

			// read head line
			vector<string> line = readLine(in);
			if (line.empty() || line[0] != "#subject") {
				LOG(WARNING) << "Invalid file format encountered - no head line - skipping file: " << file;
				return; // skip this file
			}

			// try identifying metrics
			vector<MetricPtr> metrics;
			for(std::size_t i=2; i<line.size(); i++) {
				auto metric = Metric::getForName(line[i]);
				if (!metric) {
					LOG(WARNING) << "Unsupported metric encountered - will be ignored: " << line[i];
				}
				metrics.push_back(metric);
			}


			// ---- Data ----

			line = readLine(in);
			while(!line.empty()) {

				// make sure line starts with "RG" => rest ignored
				if (line[0] != "RG") {
					line = readLine(in);
					continue;
				}

				// read region id
				region_id region = utils::numeric_cast<region_id>(line[1]);

				// read metrics
				for(std::size_t i=2; i<line.size(); i++) {
					assert(i-2 < metrics.size() && "To long row within performance result file!");

					// check metric (skip unknown metrics)
					MetricPtr metric = metrics[i-2];
					if (!metric) {
						continue;
					}

					// convert into value and safe within result
					Quantity value(utils::numeric_cast<double>(line[i]), metric->getUnit());
					res.add(id, region, metric, value);
				}

				// go to next line
				line = readLine(in);
			}
		}

	}



	Measurements loadResults(const boost::filesystem::path& directory, const string& prefix, unsigned counterWidth) {

		Measurements res;

		// iterate through all files that match <prefix>.####
		int i = 0;
		while(true) {

			// assemble file name
			std::stringstream stream;
			stream << prefix << std::setw(counterWidth) << std::setfill('0') << i;
			auto file = directory / stream.str();

			// check whether file exists
			if (!boost::filesystem::exists(file)) {
				// we are done
				return res;
			}

			// load data from file
			loadFile(file, i, res);

			// increment counter
			++i;
		}

		// to avoid a warning
		return res;
	}



} // end namespace measure
} // end namespace driver
} // end namespace insieme
