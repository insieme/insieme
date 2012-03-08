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
#include <boost/algorithm/string.hpp>

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


		// ----------------- utilities for expression metric formulas ----------------------

		/**
		 * The following implementations are used to compose expressions describing the way
		 * measurement results are extracted from some raw data.
		 *
		 * Each of the following constructs represents a functor used for conducting the actual
		 * extraction. Further, each offers a method collecting all metrics the extraction is
		 * depending on.
		 */

		/**
		 * The functor to be used in case no aggregation is supported.
		 */
		struct none {
			Quantity operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				return Quantity::invalid(metric->getUnit());
			}
			std::set<MetricPtr> getDependencies() const { return std::set<MetricPtr>(); };
		};

		// ---- List-extracting expressions ---

		/**
		 * Simply extracts a list of Quantities from a measurement data block.
		 */
		struct list {
			MetricPtr a;												// < the metric to be extracted for a region
			list(MetricPtr a) : a(a) {}
			vector<Quantity> operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				return data.getAll(region, a);
			}
			std::set<MetricPtr> getDependencies() const { std::set<MetricPtr> res; res.insert(a); return res; };
		};

		/**
		 * Computes a list of differences.
		 */
		struct diff {
			MetricPtr a,b;
			diff(MetricPtr a, MetricPtr b) : a(a),b(b) {}
			vector<Quantity> operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				vector<Quantity> dataA = data.getAll(region, a);
				vector<Quantity> dataB = data.getAll(region, b);
				vector<Quantity> res;
				assert(dataA.size() == dataB.size() && "Expecting same sequence of values for given metrics!");
				for(std::size_t i=0; i<dataA.size(); i++) {
					res.push_back(dataA[i] - dataB[i]);
				}
				return res;
			}
			std::set<MetricPtr> getDependencies() const { std::set<MetricPtr> res; res.insert(a); res.insert(b); return res; };
		};


		// ---- List-Aggregating expressions ---

		/**
		 * Computes the sum of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template<typename T>
		struct sum_impl {
			T sub;
			sum_impl(T sub) : sub(sub) {}
			Quantity operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				// check whether there is something
				auto list = sub(data, metric, region);
				if (list.empty()) {
					return Quantity::invalid(unit);
				}

				// compute average
				Quantity res(0, metric->getUnit());
				for_each(sub(data, metric, region), [&](const Quantity& cur) {
					res += cur;
				});
				return res;
			}
			std::set<MetricPtr> getDependencies() const { return sub.getDependencies(); };
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template<typename T> sum_impl<T> sum(const T& list) { return sum_impl<T>(list); }

		/**
		 * Computes the average of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template<typename T>
		struct avg_impl {
			T sub;
			avg_impl(T sub) : sub(sub) {}
			Quantity operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				// check whether there is something
				auto list = sub(data, metric, region);
				if (list.empty()) {
					return Quantity::invalid(unit);
				}

				// compute average
				Quantity res(0, metric->getUnit());
				for_each(list, [&](const Quantity& cur) {
					res += cur;
				});
				return res / Quantity(list.size());
			}
			std::set<MetricPtr> getDependencies() const { return sub.getDependencies(); };
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template<typename T> avg_impl<T> avg(const T& list) { return avg_impl<T>(list); }

		// ----------------------------------------------------------------------------------

	}


	// -- Create pre-defined metrics ---

	// define a short-cut for the none-constructor not depending on the () construction
	#define none none()

	#define METRIC(LITERAL, NAME, UNIT, EXPR) \
		Metric _##LITERAL(NAME, UNIT, \
			EXPR, \
			EXPR.getDependencies() \
		); \
		const MetricPtr Metric::LITERAL = registerMetric(_##LITERAL);

	#include "insieme/driver/measure/metrics.def"

	// cleanup
	#undef none
	#undef METRIC


	const MetricPtr Metric::getForName(const string& name) {
		auto pos = metric_register.find(name);
		if (pos == metric_register.end()) {
			return MetricPtr();
		}
		return pos->second;
	}


	std::set<MetricPtr> getDependencyClosureLeafs(const std::vector<MetricPtr>& metrics) {
		// create resulting set
		std::set<MetricPtr> res;

		// the set of metrics already checked out
		std::set<MetricPtr> checked;

		// init metrics which's dependencies still need to be considered
		std::vector<MetricPtr> toCheck = metrics;
		while(!toCheck.empty()) {

			// check out next
			MetricPtr cur = toCheck.back();
			toCheck.pop_back();

			// if not there yet, add it and consider its dependencies
			auto pos = checked.find(cur);
			if (pos != checked.end()) {
				continue;	// has been processed before
			}

			// dependencies of current still need to be explored
			auto& subDep = cur->getDependencies();
			toCheck.insert(toCheck.end(), subDep.begin(), subDep.end());

			// add leaf dependency to the resulting set
			if (subDep.empty()) res.insert(cur);
		}

		// return set containing closure of leafs
		return res;
	}

	// -- measurements --


	Quantity measure(const core::StatementAddress& stmt, const MetricPtr& metric, const ExecutorPtr& executor) {
		return measure(stmt, toVector(metric), executor)[metric];
	}

	std::map<MetricPtr, Quantity> measure(const core::StatementAddress& stmt, const vector<MetricPtr>& metrics, const ExecutorPtr& executor) {

		// pack given stmt pointer into region map
		std::map<core::StatementAddress, region_id> regions;
		regions[stmt] = 0;

		// measure region and return result
		return measure(regions, metrics, executor)[0];
	}

	vector<std::map<MetricPtr, Quantity>> measure(const core::StatementAddress& stmt, const vector<MetricPtr>& metrics,
			unsigned numRuns, const ExecutorPtr& executor) {

		// pack given stmt pointer into region map
		std::map<core::StatementAddress, region_id> regions;
		regions[stmt] = 0;

		// measure region and return result
		vector<std::map<MetricPtr, Quantity>> res;
		for_each(measure(regions, metrics, numRuns, executor), [&](const std::map<region_id, std::map<MetricPtr, Quantity>>& cur) {
			res.push_back(cur.find(0)->second);
		});
		return res;
	}


	std::map<region_id, std::map<MetricPtr, Quantity>> measure(
			const std::map<core::StatementAddress, region_id>& regions,
			const vector<MetricPtr>& metrices, const ExecutorPtr& executor) {

		return measure(regions, metrices, 1, executor)[0];
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


		std::string getPapiCounterSelector(const vector<MetricPtr>& metric) {

			// compute closure
			std::set<MetricPtr> dep;
			for_each(getDependencyClosureLeafs(metric), [&](const MetricPtr& cur) {
				if (boost::algorithm::starts_with(cur->getName(), "PAPI")) dep.insert(cur);
			});

			std::stringstream res;
			res << "\"" << join(" ", dep, [](std::ostream& out, const MetricPtr& cur) {
				out << cur->getName();
			}) << "\"";
			return res.str();
		}

	}


	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(
			const std::map<core::StatementAddress, region_id>& regions,
			const vector<MetricPtr>& metrices, unsigned numRuns, const ExecutorPtr& executor) {


		// TODO:
		// - instrument regions
		// - limit execution !!!
		// - create binary
		// - run binary
		// - collect data
		// - delete files


		// fast exit if no regions are specified or no runs have to be conducted
		if (regions.empty() || numRuns == 0) {
			return vector<std::map<region_id, std::map<MetricPtr, Quantity>>>(numRuns);
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

		// assemple PAPI-counter environment variable
		std::map<string,string> env;
		env["IRT_INST_PAPI_EVENTS"] = getPapiCounterSelector(metrices);

		// run experiments and collect results
		vector<std::map<region_id, std::map<MetricPtr, Quantity>>> res;
		for(unsigned i = 0; i<numRuns; i++) {

			// delete local files (if already present before running testrun)
			if (boost::filesystem::exists("worker_performance_log.0000")) {
				system("rm worker_*");
			}

			// run code
			// TODO: pass environment variables in a structures way
			int ret = executor->run(binFile);
			if (ret != 0) {
				throw MeasureException("Unable to run executable for measurements!");
			}

			// load data
			Measurements data = loadResults(".");

			// delete local files
			if (boost::filesystem::exists("worker_performance_log.0000")) {
				system("rm worker_*");
			}

			// collect requested information
			res.push_back(std::map<region_id, std::map<MetricPtr, Quantity>>());
			auto& curRes = res.back();
			for_each(regionIDs, [&](const region_id& region) {
				for_each(metrices, [&](const MetricPtr& metric) {
					// use extractor of metric to collect values
					curRes[region][metric] = metric->extract(data, region);
				});
			});
		}

		// delete binary
		if (boost::filesystem::exists(binFile)) {
			boost::filesystem::remove(binFile);
		}

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
