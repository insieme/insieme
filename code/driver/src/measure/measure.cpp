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
#include <functional>

#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "insieme/driver/driver_config.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/attributes.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace driver {
namespace measure {

	namespace bfs = boost::filesystem;

	// -- metrics --

	namespace {

		std::map<string, MetricPtr> metric_register;
		std::vector<MetricPtr> metric_list;

		MetricPtr registerMetric(const Metric& metric) {
			metric_register[metric.getName()] = &metric;
			metric_list.push_back(&metric);
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


		template<typename Op>
		struct pointwise {
			vector<Quantity> operator()(const vector<Quantity>& a, const vector<Quantity>& b) const {
				vector<Quantity> res;
				Op op;
				assert(a.size() == b.size() && "Expecting same sequence of values for given metrics!");
				for(std::size_t i=0; i<a.size(); i++) {
					res.push_back(op(a[i], b[i]));
				}
				return res;
			}
		};


		// ---- Derived Metric Aggregation ----

		/**
		 * A simple aggregator just representing a simple metric.
		 */
		struct id {
			MetricPtr metric;
			id(MetricPtr metric) : metric(metric) {}
			Quantity operator()(const Measurements& data, MetricPtr, region_id region) const {
				return metric->extract(data, region);
			}
			std::set<MetricPtr> getDependencies() const {
				return utils::set::toSet<std::set<MetricPtr>>(metric);
			};
		};


		/**
		 * A base type for simple, binary aggregation operators.
		 */
		template<
			typename Operator,
			typename SourceA,
			typename SourceB
		>
		struct binary_connector {
			SourceA a;
			SourceB b;
			binary_connector(const SourceA& a, const SourceB& b) : a(a), b(b) {}


			typename lambda_traits<Operator>::result_type operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				Operator op;
				return op(a(data, metric, region), b(data, metric, region));
			}

			std::set<MetricPtr> getDependencies() const {
				std::set<MetricPtr> res = a.getDependencies();
				auto dep_b = b.getDependencies();
				res.insert(dep_b.begin(), dep_b.end());
				return res;
			};
		};

		template<typename Operator, typename Source>
		struct binary_connector<Operator, Source, MetricPtr> : public binary_connector<Operator, Source, id> {
			binary_connector(const Source& a, MetricPtr b) : binary_connector<Operator, Source, id>(a, id(b)) {};
		};

		template<typename Operator, typename Source>
		struct binary_connector<Operator, MetricPtr, Source> : public binary_connector<Operator, id, Source> {
			binary_connector(MetricPtr a, const Source& b) : binary_connector<Operator, id, Source>(id(a), b) {};
		};

		template<typename Operator>
		struct binary_connector<Operator, MetricPtr, MetricPtr> : public binary_connector<Operator, id, id> {
			binary_connector(MetricPtr a, MetricPtr b) : binary_connector<Operator, id, id>(id(a), id(b)) {};
		};


		/**
		 * Computes the sum of two aggregators.
		 */
		template<typename SourceA, typename SourceB>
		binary_connector<std::plus<Quantity>, SourceA, SourceB> add(const SourceA& a, const SourceB& b) {
			return binary_connector<std::plus<Quantity>, SourceA, SourceB>(a,b);
		}

		/**
		 * Computes the product of two region metrics.
		 */
		template<typename SourceA, typename SourceB>
		binary_connector<std::minus<Quantity>, SourceA, SourceB> sub(const SourceA& a, const SourceB& b) {
			return binary_connector<std::minus<Quantity>, SourceA, SourceB>(a,b);
		}

		/**
		 * Computes the product of two region metrics.
		 */
		template<typename SourceA, typename SourceB>
		binary_connector<std::multiplies<Quantity>, SourceA, SourceB> mul(const SourceA& a, const SourceB& b) {
			return binary_connector<std::multiplies<Quantity>, SourceA, SourceB>(a,b);
		}

		/**
		 * Computes the quotient of two region metrics.
		 */
		template<typename SourceA, typename SourceB>
		binary_connector<std::divides<Quantity>, SourceA, SourceB> div(const SourceA& a, const SourceB& b) {
			return binary_connector<std::divides<Quantity>, SourceA, SourceB>(a,b);
		}



		// ---- List2List - Aggregating expressions ---

		template<typename Operator, typename SourceA, typename SourceB>
		struct binary_list_connector : public binary_connector<Operator, SourceA, SourceB> {};

		template<typename Operator, typename Source>
		struct binary_list_connector<Operator, Source, MetricPtr> : public binary_connector<Operator, Source, list> {
			binary_list_connector(const Source& a, MetricPtr b) : binary_connector<Operator, Source, list>(a, list(b)) {};
		};

		template<typename Operator, typename Source>
		struct binary_list_connector<Operator, MetricPtr, Source> : public binary_connector<Operator, list, Source> {
			binary_list_connector(MetricPtr a, const Source& b) : binary_connector<Operator, list, Source>(list(a), b) {};
		};

		template<typename Operator>
		struct binary_list_connector<Operator, MetricPtr, MetricPtr> : public binary_connector<Operator, list, list> {
			binary_list_connector(MetricPtr a, MetricPtr b) : binary_connector<Operator, list, list>(list(a), list(b)) {};
		};

		/**
		 * Computes a list of sums.
		 */
		template<typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::plus<Quantity>>, SourceA, SourceB> l_add(const SourceA& a, const SourceB& b)  {
			return binary_list_connector<pointwise<std::plus<Quantity>>, SourceA, SourceB>(a, b);
		};

		/**
		 * Computes a list of differences.
		 */
		template<typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::minus<Quantity>>, SourceA, SourceB> l_sub(const SourceA& a, const SourceB& b)  {
			return binary_list_connector<pointwise<std::minus<Quantity>>, SourceA, SourceB>(a, b);
		};

		/**
		 * Computes a list of products.
		 */
		template<typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::multiplies<Quantity>>, SourceA, SourceB> l_mul(const SourceA& a, const SourceB& b)  {
			return binary_list_connector<pointwise<std::multiplies<Quantity>>, SourceA, SourceB>(a, b);
		};

		/**
		 * Computes a list of quotients.
		 */
		template<typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::divides<Quantity>>, SourceA, SourceB> l_div(const SourceA& a, const SourceB& b)  {
			return binary_list_connector<pointwise<std::divides<Quantity>>, SourceA, SourceB>(a, b);
		};



		// ---- List-Aggregating expressions ---

		/**
		 * Computes the sum of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template<typename T>
		struct sum_impl {
			T list_extractor;
			sum_impl(T list_extractor) : list_extractor(list_extractor) {}
			Quantity operator()(const Measurements& data, MetricPtr metric, region_id region) const {
				// check whether there is something
				vector<Quantity> list = list_extractor(data, metric, region);
				if (list.empty()) {
					return Quantity::invalid(metric->getUnit());
				}

				// compute average
				Quantity res(0, list[0].getUnit());
				for_each(list, [&](const Quantity& cur) {
					res += cur;
				});
				return res;
			}
			std::set<MetricPtr> getDependencies() const { return list_extractor.getDependencies(); };
		};

		// a specialization for metric pointer
		template<> struct sum_impl<MetricPtr> : public sum_impl<list> {
			sum_impl(MetricPtr m) : sum_impl<list>(list(m)) {}
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
				Quantity res(0, list[0].getUnit());
				for_each(list, [&](const Quantity& cur) {
					res += cur;
				});
				return res / Quantity(list.size());
			}
			std::set<MetricPtr> getDependencies() const { return sub.getDependencies(); };
		};

		// a specialization for metric pointer
		template<> struct avg_impl<MetricPtr> : public avg_impl<list> {
			avg_impl(MetricPtr m) : avg_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template<typename T> avg_impl<T> avg(const T& list) { return avg_impl<T>(list); }

		// ----------------------------------------------------------------------------------

	}


	// -- Create pre-defined metrics ---

	// define a short-cut for the none-constructor such that the empty call none() doesn't have to be used
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

	const vector<MetricPtr>& Metric::getAll() {
		return metric_list;
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

	utils::compiler::Compiler getDefaultCompilerForMeasurments() {
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();

		compiler = utils::compiler::Compiler("gcc");

		// enable optimization
		compiler.addFlag("--std=c99");
		compiler.addFlag("-O3");

		// the rest is done by the measurement procedure
		return compiler;
	}

	Quantity measure(const core::StatementAddress& stmt, const MetricPtr& metric, const ExecutorPtr& executor, const utils::compiler::Compiler& compiler) {
		return measure(stmt, toVector(metric), executor, compiler)[metric];
	}

	std::map<MetricPtr, Quantity> measure(const core::StatementAddress& stmt, const vector<MetricPtr>& metrics,
			const ExecutorPtr& executor, const utils::compiler::Compiler& compiler) {

		// pack given stmt pointer into region map
		std::map<core::StatementAddress, region_id> regions;
		regions[stmt] = 0;

		// measure region and return result
		return measure(regions, metrics, executor, compiler)[0];
	}

	vector<std::map<MetricPtr, Quantity>> measure(const core::StatementAddress& stmt, const vector<MetricPtr>& metrics,
			unsigned numRuns, const ExecutorPtr& executor, const utils::compiler::Compiler& compiler) {

		// pack given stmt pointer into region map
		std::map<core::StatementAddress, region_id> regions;
		regions[stmt] = 0;

		// measure region and return result
		vector<std::map<MetricPtr, Quantity>> res;
		for_each(measure(regions, metrics, numRuns, executor, compiler), [&](const std::map<region_id, std::map<MetricPtr, Quantity>>& cur) {
			res.push_back(cur.find(0)->second);
		});
		return res;
	}


	std::map<region_id, std::map<MetricPtr, Quantity>> measure(
			const std::map<core::StatementAddress, region_id>& regions,
			const vector<MetricPtr>& metrices, const ExecutorPtr& executor,
			const utils::compiler::Compiler& compiler) {

		return measure(regions, metrices, 1, executor, compiler)[0];
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
			const auto& basic = manager.getLangBasic();
			const auto& unit = basic.getUnit();
			auto& rtExt = manager.getLangExtension<insieme::backend::runtime::Extensions>();

			// convert region id to IR id
			auto regionID = build.uintLit(id);

			// check whether instrumented target is a pfor call
			if (stmt->getNodeCategory() == core::NC_Expression) {
				const core::ExpressionPtr& expr = stmt.as<core::ExpressionPtr>();
				if (core::analysis::isCallOf(core::analysis::stripAttributes(expr), basic.getPFor())) {
					const core::CallExprPtr& call = expr.as<core::CallExprPtr>();

					// build attribute to be added
					auto mark = build.callExpr(rtExt.regionAttribute, regionID);

					// create attributed version of pfor call
					return core::transform::replaceNode(manager, core::CallExprAddress(call)->getFunctionExpr(),
							core::analysis::addAttribute(call->getFunctionExpr(), mark)).as<core::StatementPtr>();
				}
			}

			// build instrumented code section using begin/end markers
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


		vector<vector<MetricPtr>> partitionPapiCounter(const vector<MetricPtr>& metric) {

			// compute closure and filter out PAPI counters
			std::set<MetricPtr> dep;
			for_each(getDependencyClosureLeafs(metric), [&](const MetricPtr& cur) {
				if (boost::algorithm::starts_with(cur->getName(), "PAPI")) dep.insert(cur);
			});

			// pack counters into groups (TODO: use PAPI library to ensure combinations are valid)
			vector<vector<MetricPtr>> res;
			res.push_back(vector<MetricPtr>());
			for (auto cur = dep.begin(); cur != dep.end(); cur++) {
				if (res.back().size() == 1u) {		// 1 to be sure to avoid conflicts for now
					res.push_back(vector<MetricPtr>());
				}
				res.back().push_back(*cur);
			}

			return res;
		}

		std::string getPapiCounterSelector(const vector<MetricPtr>& metric) {

			// compute closure
			std::set<MetricPtr> dep;
			for_each(getDependencyClosureLeafs(metric), [&](const MetricPtr& cur) {
				if (boost::algorithm::starts_with(cur->getName(), "PAPI")) dep.insert(cur);
			});

			std::stringstream res;
			res << join(":", dep, [](std::ostream& out, const MetricPtr& cur) {
				out << cur->getName();
			});
			return res.str();
		}

	}


	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(
			const std::map<core::StatementAddress, region_id>& regions,
			const vector<MetricPtr>& metrics, unsigned numRuns,
			const ExecutorPtr& executor, const utils::compiler::Compiler& compiler) {

		// fast exit if no regions are specified or no runs have to be conducted
		if (regions.empty() || numRuns == 0) {
			return vector<std::map<region_id, std::map<MetricPtr, Quantity>>>(numRuns);
		}

		// create the instrumented binary
		auto binFile = buildBinary(regions, compiler);
		if (binFile.empty()) {
			throw MeasureException("Unable to compiling executable for measurement!");
		}

		// conduct measurement
		auto res = measure(binFile, metrics, numRuns, executor);

		// delete binary
		if (boost::filesystem::exists(binFile)) {
			boost::filesystem::remove(binFile);
		}

		return res;
	}

	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(
			const std::string& binary, const vector<MetricPtr>& metrics,
			unsigned numRuns, const ExecutorPtr& executor, const std::map<string, string>& env) {

		// check binary
		if (!boost::filesystem::exists(binary)) {
			throw MeasureException("Invalid executable specified for measurement!");
		}

		// extract name of executable
		std::string executable = bfs::path(binary).filename().string();

		// partition the papi parameters
		auto papiPartition = partitionPapiCounter(metrics);

		// run experiments and collect results
		vector<std::map<region_id, std::map<MetricPtr, Quantity>>> res;
		for(unsigned i = 0; i<numRuns; i++) {

			// the data to be collected for this run
			Measurements data;

			// run once for each parameter sub-set computed by the partitioning
			for_each(papiPartition, [&](const vector<MetricPtr>& paramList) {

				// create a directory
				auto workdir = bfs::path(".") / ("work_dir_" + executable);
				assert(!bfs::exists(workdir) && "Working-Directory already present!");

				// assemble PAPI-counter environment variable
				std::map<string,string> mod_env = env;
				if (!paramList.empty()) {		// only set if there are any parameters (otherwise collection is disabled)
					mod_env["IRT_INST_PAPI_EVENTS"] = getPapiCounterSelector(paramList);
				}

				// run code
				int ret = executor->run(binary, mod_env, workdir.string());
				if (ret != 0) {
					throw MeasureException("Unable to run executable for measurements!");
				}

				// load data and merge it
				data.mergeIn(loadResults(workdir));

				// delete local files
				if (boost::filesystem::exists(workdir)) {
					bfs::remove_all(workdir);
				}

			});


			// extract results
			res.push_back(std::map<region_id, std::map<MetricPtr, Quantity>>());
			auto& curRes = res.back();
			for_each(data.getAllRegions(), [&](const region_id& region) {
				for_each(metrics, [&](const MetricPtr& metric) {
					// use extractor of metric to collect values
					curRes[region][metric] = metric->extract(data, region);
				});
			});

		}

		// return result
		return res;
	}

	std::string buildBinary(const core::StatementAddress& region, const utils::compiler::Compiler& compiler) {
		std::map<core::StatementAddress, region_id> regions;
		regions[region] = 0;
		return buildBinary(regions, compiler);
	}


	std::string buildBinary(const std::map<core::StatementAddress, region_id>& regions,
			const utils::compiler::Compiler& compilerSetup) {

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

		// customize compiler
		utils::compiler::Compiler compiler = compilerSetup;

		// add flags required by the runtime
		compiler.addFlag("-I " SRC_DIR "../../runtime/include");
		compiler.addFlag("-I " PAPI_HOME "/include");
		compiler.addFlag("-L " PAPI_HOME "/lib/");
		compiler.addFlag("-D_XOPEN_SOURCE=700 -D_GNU_SOURCE");
		compiler.addFlag("-DIRT_ENABLE_REGION_INSTRUMENTATION");
		compiler.addFlag("-DIRT_RUNTIME_TUNING");
		compiler.addFlag("-ldl -lrt -lpthread -lm");
		compiler.addFlag("-Wl,-Bstatic -lpapi -Wl,-Bdynamic");

		// compile code to binary
		return utils::compiler::compileToBinary(*targetCode, compiler);
	}




	void Measurements::add(worker_id worker, region_id region, MetricPtr metric, const Quantity& value) {
		workerDataStore[worker][region][metric].push_back(value);	// just add value
	}

	void Measurements::add(region_id region, MetricPtr metric, const Quantity& value) {
		regionDataStore[region][metric].push_back(value);
	}

	void Measurements::mergeIn(const Measurements& other) {

		// a lambda merging region data
		static auto mergeRegionData = [](RegionDataStore& trg, const RegionDataStore& src) {
			for_each(src, [&](const RegionDataStore::value_type& value) {
				region_id region = value.first;
				for_each(value.second, [&](const std::pair<MetricPtr, vector<Quantity>>& inner) {
					vector<Quantity>& list = trg[region][inner.first];
					if (list.empty()) {
						list.insert(list.end(), inner.second.begin(), inner.second.end());
					}
				});
			});
		};

		// merge the region data
		mergeRegionData(regionDataStore, other.regionDataStore);

		// merge the region data structures within the worker data stores
		for_each(other.workerDataStore, [&](const WorkerDataStore::value_type& value) {
			mergeRegionData(workerDataStore[value.first], value.second);
		});

	}

	vector<Quantity> Measurements::getAll(region_id region, MetricPtr metric) const {
		vector<Quantity> res;

		// a lambda merging region data
		auto collectRegionData = [&](const RegionDataStore& src) {
			for_each(src, [&](const pair<region_id, std::map<MetricPtr, vector<Quantity>>>& cur) {
				if (cur.first == region) {
					auto pos = cur.second.find(metric);
					if (pos != cur.second.end()) {
						const vector<Quantity>& list = pos->second;
						res.insert(res.end(), list.begin(), list.end());
					}
				}
			});
		};

		// collect data from region store
		collectRegionData(regionDataStore);

		// collect data from worker store
		for_each(workerDataStore, [&](const WorkerDataStore::value_type& value) {
			collectRegionData(value.second);
		});

		return res;
	}

	std::set<region_id> Measurements::getAllRegions() const {
		std::set<region_id> res;

		auto collectRegions = [&](const RegionDataStore& regionStore) {
			for_each(regionStore, [&](const RegionDataStore::value_type& cur) {
				res.insert(cur.first);
			});
		};

		collectRegions(regionDataStore);

		for_each(workerDataStore, [&](const WorkerDataStore::value_type& value) {
			collectRegions(value.second);
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

		void loadFile(const boost::filesystem::path& file,
				const std::function<void(region_id id, MetricPtr metric, const Quantity& value)>& add) {

			// check whether file exists
			if (!boost::filesystem::exists(file)) return;

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
					add(region, metric, value);
				}

				// go to next line
				line = readLine(in);
			}
		}

	}



	Measurements loadResults(const boost::filesystem::path& directory) {

		Measurements res;

		// iterate through all performance log files
		int i = 0;
		while(true) {

			// assemble file name
			std::stringstream stream;
			stream << "worker_performance_log." << std::setw(4) << std::setfill('0') << i;
			auto file = directory / stream.str();

			// check whether file exists
			if (!boost::filesystem::exists(file)) {
				// we are done
				break;
			}

			// load data from file
			loadFile(file, [&](region_id region, MetricPtr metric, const Quantity& value) {
				res.add(i, region, metric, value);
			});

			// increment counter
			++i;
		}


		// load the efficiency log
		loadFile(directory / "worker_efficiency_log",
			[&](region_id region, MetricPtr metric, const Quantity& value) {
				res.add(region, metric, value);
			}
		);

		// to avoid a warning
		return res;
	}



} // end namespace measure
} // end namespace driver
} // end namespace insieme
