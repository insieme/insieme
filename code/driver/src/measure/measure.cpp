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

#include "insieme/driver/measure/measure.h"

#include <set>
#include <map>
#include <functional>

#include <boost/tokenizer.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <insieme/backend/runtime/runtime_extension.h>

#include "insieme/utils/config.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/region/for_selector.h"
#include "insieme/core/analysis/region/pfor_selector.h"
#include "insieme/core/lang/instrumentation_extension.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace driver {
namespace measure {

	using std::pair;

	namespace bfs = boost::filesystem;

	// -- metrics --

	namespace {

		std::map<string, MetricPtr> metric_register;
		std::vector<MetricPtr> metric_list;

		MetricPtr registerMetric(const Metric& metric) {
			std::stringstream stream;
			stream << metric << "(" << *(metric.getUnit()) << ")";
			metric_register[stream.str()] = &metric;
			metric_list.push_back(&metric);
			return &metric;
		}


		// --- units to be used within the metrics ---

		auto ns = makeUnitPtr(nano * s);
		auto kb = makeUnitPtr(kilo * byte);
		auto j = makeUnitPtr((kg * (m ^ 2)) / (s ^ 2)); // 1 joule
		auto cycle = makeUnitPtr(Unit("cycle"));
		auto c = makeUnitPtr(celsius);
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
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// slightly hacky: the none() extractor can be used for metrics that do not require any
				// aggregation, i.e. when a metric occurs exactly once - for metrics occurring more often,
				// we'd lose information and hence bail out with an invalid quantity as a result
				vector<Quantity> res = data.getAll(region, metric);
				if(res.size() == 1) {
					return res[0];
				} else {
					LOG(WARNING) << "'none' functor only allows single quantities, but found " << res.size() << " for metric " << metric;
					return Quantity::invalid(metric->getUnit());
				}
			}
			std::set<MetricPtr> getDependencies() const { return std::set<MetricPtr>(); };
		};

		// ---- List-extracting expressions ---

		/**
		 * Simply extracts a list of Quantities from a measurement data block.
		 */
		struct list {
			MetricPtr a; // < the metric to be extracted for a region
			list(MetricPtr a) : a(a) {}
			vector<Quantity> operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				return data.getAll(region, a);
			}
			std::set<MetricPtr> getDependencies() const {
				std::set<MetricPtr> res;
				res.insert(a);
				return res;
			};
		};


		template <typename Op>
		struct pointwise {
			vector<Quantity> operator()(const vector<Quantity>& a, const vector<Quantity>& b) const {
				vector<Quantity> res;
				Op op;
				assert_eq(a.size(), b.size()) << "Expecting same sequence of values for given metrics!";
				for(std::size_t i = 0; i < a.size(); i++) {
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
			Quantity operator()(const Measurements& data, const MetricPtr, region_id region) const {
				return metric->extract(data, region);
			}
			std::set<MetricPtr> getDependencies() const {
				return utils::set::toSet<std::set<MetricPtr>>(metric);
			};
		};


		/**
		 * A base type for simple, binary aggregation operators.
		 */
		template <typename Operator, typename SourceA, typename SourceB>
		struct binary_connector {
			SourceA a;
			SourceB b;
			binary_connector(const SourceA& a, const SourceB& b) : a(a), b(b) {}


			typename lambda_traits<Operator>::result_type operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
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

		template <typename Operator, typename Source>
		struct binary_connector<Operator, Source, MetricPtr> : public binary_connector<Operator, Source, id> {
			binary_connector(const Source& a, MetricPtr b) : binary_connector<Operator, Source, id>(a, id(b)){};
		};

		template <typename Operator, typename Source>
		struct binary_connector<Operator, MetricPtr, Source> : public binary_connector<Operator, id, Source> {
			binary_connector(MetricPtr a, const Source& b) : binary_connector<Operator, id, Source>(id(a), b){};
		};

		template <typename Operator>
		struct binary_connector<Operator, MetricPtr, MetricPtr> : public binary_connector<Operator, id, id> {
			binary_connector(MetricPtr a, MetricPtr b) : binary_connector<Operator, id, id>(id(a), id(b)){};
		};


		/**
		 * Computes the sum of two aggregators.
		 */
		template <typename SourceA, typename SourceB>
		binary_connector<std::plus<Quantity>, SourceA, SourceB> add(const SourceA& a, const SourceB& b) {
			return binary_connector<std::plus<Quantity>, SourceA, SourceB>(a, b);
		}

		/**
		 * Computes the difference of two region metrics.
		 */
		template <typename SourceA, typename SourceB>
		binary_connector<std::minus<Quantity>, SourceA, SourceB> sub(const SourceA& a, const SourceB& b) {
			return binary_connector<std::minus<Quantity>, SourceA, SourceB>(a, b);
		}

		/**
		 * Computes the product of two region metrics.
		 */
		template <typename SourceA, typename SourceB>
		binary_connector<std::multiplies<Quantity>, SourceA, SourceB> mul(const SourceA& a, const SourceB& b) {
			return binary_connector<std::multiplies<Quantity>, SourceA, SourceB>(a, b);
		}

		/**
		 * Computes the quotient of two region metrics.
		 */
		template <typename SourceA, typename SourceB>
		binary_connector<std::divides<Quantity>, SourceA, SourceB> div(const SourceA& a, const SourceB& b) {
			return binary_connector<std::divides<Quantity>, SourceA, SourceB>(a, b);
		}


		// ---- List2List - Aggregating expressions ---

		template <typename Operator, typename SourceA, typename SourceB>
		struct binary_list_connector : public binary_connector<Operator, SourceA, SourceB> {};

		template <typename Operator, typename Source>
		struct binary_list_connector<Operator, Source, MetricPtr> : public binary_connector<Operator, Source, list> {
			binary_list_connector(const Source& a, MetricPtr b) : binary_connector<Operator, Source, list>(a, list(b)){};
		};

		template <typename Operator, typename Source>
		struct binary_list_connector<Operator, MetricPtr, Source> : public binary_connector<Operator, list, Source> {
			binary_list_connector(MetricPtr a, const Source& b) : binary_connector<Operator, list, Source>(list(a), b){};
		};

		template <typename Operator>
		struct binary_list_connector<Operator, MetricPtr, MetricPtr> : public binary_connector<Operator, list, list> {
			binary_list_connector(MetricPtr a, MetricPtr b) : binary_connector<Operator, list, list>(list(a), list(b)){};
		};

		/**
		 * Computes a list of sums.
		 */
		template <typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::plus<Quantity>>, SourceA, SourceB> l_add(const SourceA& a, const SourceB& b) {
			return binary_list_connector<pointwise<std::plus<Quantity>>, SourceA, SourceB>(a, b);
		};

		/**
		 * Computes a list of differences.
		 */
		template <typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::minus<Quantity>>, SourceA, SourceB> l_sub(const SourceA& a, const SourceB& b) {
			return binary_list_connector<pointwise<std::minus<Quantity>>, SourceA, SourceB>(a, b);
		};

		/**
		 * Computes a list of products.
		 */
		template <typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::multiplies<Quantity>>, SourceA, SourceB> l_mul(const SourceA& a, const SourceB& b) {
			return binary_list_connector<pointwise<std::multiplies<Quantity>>, SourceA, SourceB>(a, b);
		};

		/**
		 * Computes a list of quotients.
		 */
		template <typename SourceA, typename SourceB>
		binary_list_connector<pointwise<std::divides<Quantity>>, SourceA, SourceB> l_div(const SourceA& a, const SourceB& b) {
			return binary_list_connector<pointwise<std::divides<Quantity>>, SourceA, SourceB>(a, b);
		};


		// ---- List-Aggregating expressions ---

		/**
		 *  Computes the first of quantities extracted as a list.
		 *
		 *  @param T the functor used to extract the list.
		 */
		template <typename T>
		struct first_impl {
			T list_extractor;
			first_impl(T list_extractor) : list_extractor(list_extractor) {}
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// check whether there is something
				vector<Quantity> list = list_extractor(data, metric, region);
				if(list.empty()) { return Quantity::invalid(metric->getUnit()); }

				// get first
				Quantity res(0, list[0].getUnit());
				res = list[0];

				return res;
			}
			std::set<MetricPtr> getDependencies() const {
				return list_extractor.getDependencies();
			};
		};

		// a specialization for metric pointer
		template <>
		struct first_impl<MetricPtr> : public first_impl<list> {
			first_impl(MetricPtr m) : first_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template <typename T>
		first_impl<T> first(const T& list) {
			return first_impl<T>(list);
		}

		/**
		 *  Computes the standard deviation of quantities extracted as a list.
		 *
		 *  @param T the functor used to extract the list.
		 */
		template <typename T>
		struct var_impl {
			T list_extractor;
			var_impl(T list_extractor) : list_extractor(list_extractor) {}
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// check whether there is something
				vector<Quantity> list = list_extractor(data, metric, region);
				if(list.empty()) { return Quantity::invalid(metric->getUnit()); }

				// get sum
				Quantity res(0, list[0].getUnit());
				for_each(list, [&](const Quantity& cur) { res += cur; });

				// compute average
				Quantity average = res / Quantity(list.size());

				auto ns2 = makeUnitPtr((nano * s) ^ 2);

				// compute variance
				Quantity varianceTemp(0, ns2);
				for_each(list, [&](const Quantity& cur) { varianceTemp += Quantity(cur - average) * Quantity(cur - average); });

				return varianceTemp / average;
			}
			std::set<MetricPtr> getDependencies() const {
				return list_extractor.getDependencies();
			};
		};

		// a specialization for metric pointer
		template <>
		struct var_impl<MetricPtr> : public var_impl<list> {
			var_impl(MetricPtr m) : var_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template <typename T>
		var_impl<T> var(const T& list) {
			return var_impl<T>(list);
		}

		/**
		 * Computes the min of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template <typename T>
		struct min_impl {
			T list_extractor;
			min_impl(T list_extractor) : list_extractor(list_extractor) {}
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// check whether there is something
				vector<Quantity> list = list_extractor(data, metric, region);
				if(list.empty()) { return Quantity::invalid(metric->getUnit()); }

				// compute min
				Quantity res = list[0];
				for_each(list, [&](const Quantity& cur) {
					if(cur < res) { res = cur; }
				});
				return res;
			}
			std::set<MetricPtr> getDependencies() const {
				return list_extractor.getDependencies();
			};
		};

		// a specialization for metric pointer
		template <>
		struct min_impl<MetricPtr> : public min_impl<list> {
			min_impl(MetricPtr m) : min_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template <typename T>
		min_impl<T> min(const T& list) {
			return min_impl<T>(list);
		}

		/**
		 * Computes the max of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template <typename T>
		struct max_impl {
			T list_extractor;
			max_impl(T list_extractor) : list_extractor(list_extractor) {}
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// check whether there is something
				vector<Quantity> list = list_extractor(data, metric, region);
				if(list.empty()) { return Quantity::invalid(metric->getUnit()); }

				// compute max
				Quantity res = list[0];
				for_each(list, [&](const Quantity& cur) {
					if(cur > res) { res = cur; }
				});
				return res;
			}
			std::set<MetricPtr> getDependencies() const {
				return list_extractor.getDependencies();
			};
		};

		// a specialization for metric pointer
		template <>
		struct max_impl<MetricPtr> : public max_impl<list> {
			max_impl(MetricPtr m) : max_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template <typename T>
		max_impl<T> max(const T& list) {
			return max_impl<T>(list);
		}

		/**
		 * Computes the sum of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template <typename T>
		struct sum_impl {
			T list_extractor;
			sum_impl(T list_extractor) : list_extractor(list_extractor) {}
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// check whether there is something
				vector<Quantity> list = list_extractor(data, metric, region);
				if(list.empty()) { return Quantity::invalid(metric->getUnit()); }

				// compute sum
				Quantity res(0, list[0].getUnit());
				for_each(list, [&](const Quantity& cur) { res += cur; });
				return res;
			}
			std::set<MetricPtr> getDependencies() const {
				return list_extractor.getDependencies();
			};
		};

		// a specialization for metric pointer
		template <>
		struct sum_impl<MetricPtr> : public sum_impl<list> {
			sum_impl(MetricPtr m) : sum_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template <typename T>
		sum_impl<T> sum(const T& list) {
			return sum_impl<T>(list);
		}

		/**
		 * Computes the average of quantities extracted as a list.
		 *
		 * @param T the functor used to extract the list.
		 */
		template <typename T>
		struct avg_impl {
			T sub;
			avg_impl(T sub) : sub(sub) {}
			Quantity operator()(const Measurements& data, const MetricPtr metric, region_id region) const {
				// check whether there is something
				auto list = sub(data, metric, region);
				if(list.empty()) { return Quantity::invalid(unit); }

				// compute average
				Quantity res(0, list[0].getUnit());
				for_each(list, [&](const Quantity& cur) { res += cur; });
				return res / Quantity(list.size());
			}
			std::set<MetricPtr> getDependencies() const {
				return sub.getDependencies();
			};
		};

		// a specialization for metric pointer
		template <>
		struct avg_impl<MetricPtr> : public avg_impl<list> {
			avg_impl(MetricPtr m) : avg_impl<list>(list(m)) {}
		};

		/**
		 * Since template-structs cannot be constructed nicely without specifying the template
		 * parameters, this function is introducing the necessary automated type deduction.
		 */
		template <typename T>
		avg_impl<T> avg(const T& list) {
			return avg_impl<T>(list);
		}

		// ----------------------------------------------------------------------------------
	}


	// -- Create pre-defined metrics ---

	// define a short-cut for the none-constructor such that the empty call none() doesn't have to be used
	#define none none()

	#define METRIC(LITERAL, NAME, UNIT, EXPR)                                                                                                                  \
		Metric _##LITERAL(NAME, UNIT, EXPR, EXPR.getDependencies());                                                                                           \
		const MetricPtr Metric::LITERAL = registerMetric(_##LITERAL);

	#include "insieme/driver/measure/metrics.def"

	// cleanup
	#undef none
	#undef METRIC


	const MetricPtr Metric::getForNameAndUnit(const string& name) {
		auto pos = metric_register.find(name);
		if(pos == metric_register.end()) { return MetricPtr(); }
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

		// init metrics whose dependencies still need to be considered
		std::vector<MetricPtr> toCheck = metrics;
		while(!toCheck.empty()) {
			// check out next
			MetricPtr cur = toCheck.back();
			toCheck.pop_back();

			// if not there yet, add it and consider its dependencies
			auto pos = checked.find(cur);
			if(pos != checked.end()) {
				continue; // has been processed before
			}

			// dependencies of current still need to be explored
			auto& subDep = cur->getDependencies();
			toCheck.insert(toCheck.end(), subDep.begin(), subDep.end());

			// add leaf dependency to the resulting set
			if(subDep.empty()) { res.insert(cur); }
		}

		LOG(INFO) << "metric dependency closure leafs: " << res;

		// return set containing closure of leafs
		return res;
	}

	// -- measurements --

	utils::compiler::Compiler getDefaultCompilerForMeasurments() {
		// just take default optimizing compiler
		auto compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		return utils::compiler::Compiler::getOptimizedCompiler(compiler);
	}

	MeasurementSetup getDefaultMeasurementSetup() {
		return MeasurementSetup();
	}

	vector<Quantity> measure(const core::StatementPtr& stmt, const MetricPtr& metric, const MeasurementSetup& setup) {
		return measure(core::StatementAddress(stmt), metric, setup);
	}

	vector<Quantity> measure(const core::StatementAddress& stmt, const MetricPtr& metric, const MeasurementSetup& setup) {
		vector<Quantity> res;
		for_each(measure(stmt, toVector(metric), setup),
		         [&](const std::map<MetricPtr, Quantity>& cur) { res.push_back(cur.find(metric)->second); });
		return res;
	}

	vector<std::map<MetricPtr, Quantity>> measure(const core::StatementAddress& stmt, const vector<MetricPtr>& metrics, const MeasurementSetup& setup) {
		// pack given stmt pointer into region map
		std::map<core::StatementAddress, region_id> regions;
		regions[stmt] = 0;

		// measure region and return result
		vector<std::map<MetricPtr, Quantity>> res;
		for_each(measure(regions, metrics, setup),
		         [&](const std::map<region_id, std::map<MetricPtr, Quantity>>& cur) { res.push_back(cur.find(0)->second); });
		return res;
	}

	vector<std::map<core::StatementAddress, std::map<MetricPtr, Quantity>>>
	measure(const vector<core::StatementAddress>& regions, const vector<MetricPtr>& metrices, const MeasurementSetup& setup) {
		// create a stmt-address <-> region_id map
		std::map<core::StatementAddress, region_id> mappedRegions;
		region_id id = 0;
		for(const auto& cur : regions) {
			mappedRegions[cur] = id++;
		}

		// run measurements
		auto data = measure(mappedRegions, metrices, setup);

		// un-pack results
		return ::transform(
		    data, [&](const std::map<region_id, std::map<MetricPtr, Quantity>>& data) -> std::map<core::StatementAddress, std::map<MetricPtr, Quantity>> {
			    std::map<core::StatementAddress, std::map<MetricPtr, Quantity>> res;
			    for(const auto& cur : data) {
				    res[regions[cur.first]] = cur.second;
			    }
			    return res;
			});
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
		core::StatementPtr instrumentStmt(const core::StatementPtr& stmt, region_id id) {
			// instrument region
			auto& manager = stmt->getNodeManager();
			core::IRBuilder build(manager);

			// obtain references to primitives
			const auto& basic = manager.getLangBasic();
			const auto& unit = basic.getUnit();
			auto& instExt = manager.getLangExtension<insieme::core::lang::InstrumentationExtension>();

			// convert region id to IR id
			auto regionID = build.uintLit(id);

			// check whether instrumented target is a pfor call
			//			if (stmt->getNodeCategory() == core::NC_Expression) {
			//				const core::ExpressionPtr& expr = stmt.as<core::ExpressionPtr>();
			//				if (core::analysis::isCallOf(core::analysis::stripAttributes(expr), basic.getPFor())) {
			//					const core::CallExprPtr& call = expr.as<core::CallExprPtr>();
			//
			//					// build attribute to be added
			//					auto mark = build.callExpr(rtExt.regionAttribute, regionID);
			//
			//					// create attributed version of pfor call
			//					return core::transform::replaceNode(manager, core::CallExprAddress(call)->getFunctionExpr(),
			//							core::analysis::addAttribute(call->getFunctionExpr(), mark)).as<core::StatementPtr>();
			//				}
			//			}

			// build instrumented code section using begin/end markers
			auto region_inst_start_call = build.callExpr(unit, instExt.getInstrumentationRegionStart(), regionID);
			auto region_inst_end_call = build.callExpr(unit, instExt.getInstrumentationRegionEnd(), regionID);

			// instrument exit points
			core::StatementPtr instrumented = stmt;
			auto exitPoints = core::analysis::getExitPoints(stmt);
			std::sort(exitPoints.rbegin(), exitPoints.rend());
			for(const core::StatementAddress& point : exitPoints) {
				// break and continue
				if(point->getNodeType() == core::NT_BreakStmt || point->getNodeType() == core::NT_ContinueStmt) {
					// insert region_end call before statement
					instrumented = core::transform::insert(manager, point.switchRoot(instrumented).getParentAddress().as<core::CompoundStmtAddress>(),
					                                       region_inst_end_call, point.getIndex())
					                   .as<core::StatementPtr>();
					continue;
				}

				// handle return statement - TODO: add support for exceptions
				assert_eq(point->getNodeType(), core::NT_ReturnStmt) << "Only break, continue and return should constitute a exit point!";

				core::ReturnStmtPtr ret = point.as<core::ReturnStmtPtr>();
				core::ExpressionPtr retVal = ret->getReturnExpr();

				core::StatementList stmts;
				if(retVal->getNodeType() == core::NT_Variable || retVal->getNodeType() == core::NT_Literal) {
					// no modification necessary
					stmts.push_back(region_inst_end_call);
					stmts.push_back(ret);
				} else {
					// separate result computation from return
					core::VariablePtr var = build.variable(retVal->getType());
					stmts.push_back(build.declarationStmt(var, retVal));
					stmts.push_back(region_inst_end_call);
					stmts.push_back(build.returnStmt(var));
				}

				// build replacement
				instrumented = core::transform::replaceNode(manager, point, build.compoundStmt(stmts)).as<core::StatementPtr>();
			}

			// assemble substitution
			return build.compoundStmt(region_inst_start_call, instrumented, region_inst_end_call);
		}

		/**
		* This function instrumented a given map of addresses and region IDs
		*
		* @param regions the regions to be instrumented with the corresponding region IDs
		* @return the root node of an IR holding instrumented versions of all regions
		*/
		core::NodePtr instrumentRegions(const std::map<core::StatementAddress, region_id>& regions) {
			using region_pair = std::pair<core::StatementAddress, region_id>;
			core::NodePtr root = regions.begin()->first.getRootNode();
			core::NodeManager& manager = root->getNodeManager();

			// sort addresses in descending order
			vector<region_pair> sorted_regions(regions.begin(), regions.end());
			std::sort(sorted_regions.rbegin(), sorted_regions.rend());

			// replace all regions with optimized versions
			for_each(sorted_regions, [&](const pair<core::StatementAddress, region_id>& cur) {
				// obtain address with current root
				core::StatementAddress tmp = cur.first.switchRoot(root);
				// migrate autotuning information (and other annotations if present)
				core::transform::utils::migrateAnnotations(cur.first, tmp);
				// replace region
				root = core::transform::replaceNode(manager, tmp, cur.first);
			});

			// replace all regions with instrumented versions
			for_each(sorted_regions, [&](const pair<core::StatementAddress, region_id>& cur) {
				// obtain address with current root
				core::StatementAddress tmp = cur.first.switchRoot(root);
				// instrument the new region
				core::StatementPtr instrumentedTmp = instrumentStmt(tmp, cur.second);
				// replace region
				root = core::transform::replaceNode(manager, tmp, instrumentedTmp);
			});

			return root;
		}

		/**
		 * Wraps the given code fragment into a program - to be processed by the backend.
		 */
		core::ProgramPtr wrapIntoProgram(const core::NodePtr& ptr) {
			// check whether it is already a program
			if(ptr->getNodeType() == core::NT_Program) { return ptr.as<core::ProgramPtr>(); }

			auto& mgr = ptr->getNodeManager();
			core::IRBuilder builder(mgr);

			// get code fragment as a stmt
			core::StatementPtr stmt = ptr.as<core::StatementPtr>();

			// build entry point
			core::NodePtr main = builder.lambdaExpr(builder.getLangBasic().getUnit(), core::VariableList(), stmt);

			// build enclosing program
			return builder.program(toVector(main));
		}


		vector<vector<MetricPtr>> partitionPapiCounter(const vector<MetricPtr>& metric) {
			// compute closure and filter out PAPI counters
			std::set<MetricPtr> dep;
			for_each(getDependencyClosureLeafs(metric), [&](const MetricPtr& cur) {
				if(boost::algorithm::starts_with(cur->getName(), "PAPI")) { dep.insert(cur); }
			});

			// pack counters into groups (TODO: use PAPI library to ensure combinations are valid)
			vector<vector<MetricPtr>> res;
			res.push_back(vector<MetricPtr>());
			for(auto cur = dep.begin(); cur != dep.end(); cur++) {
				if(res.back().size() == 1u) { // 1 to be sure to avoid conflicts for now
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
				if(boost::algorithm::starts_with(cur->getName(), "PAPI")) { dep.insert(cur); }
			});

			std::stringstream res;
			res << join(",", dep, [](std::ostream& out, const MetricPtr& cur) { out << cur->getName(); });
			return res.str();
		}
	}


	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(const std::map<core::StatementAddress, region_id>& regions,
	                                                                   const vector<MetricPtr>& metrics, const MeasurementSetup& setup) {
		// fast exit if no regions are specified or no runs have to be conducted
		if(regions.empty() || setup.numRuns == 0) { return vector<std::map<region_id, std::map<MetricPtr, Quantity>>>(setup.numRuns); }

		auto modifiedSetup = setup;
		// fix static scheduling
		// TODO: it is no longer necessary to fix to static scheduling, should we keep it?
		modifiedSetup.compiler.addFlag("-DIRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC");

		// enable papi only if at least one metric requires it
		bool usePapi = any(getDependencyClosureLeafs(metrics), [](const MetricPtr& cur) {
			if(cur->getName().find("PAPI") != std::string::npos) { return true; }
			return false;
		});

		if(usePapi) {
			#ifndef USE_PAPI
				LOG(ERROR) << "PAPI events requested, but compiled without PAPI support";
			#else
				modifiedSetup.compiler.addFlag("-DIRT_USE_PAPI");
			#endif
		}

		// instrument code
		auto instrumentedRoot = instrumentRegions(regions);

		// build target code
		auto binFile = buildBinary(instrumentedRoot, modifiedSetup);
		assert_false(binFile.empty()) << "Unable to compile executable for measurement!";

		// conduct measurement
		auto res = measure(binFile, metrics, setup);

		// delete binary
		if(boost::filesystem::exists(binFile)) { boost::filesystem::remove(binFile); }

		return res;
	}

	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(const std::string& binary, const vector<MetricPtr>& metrics,
		                                                               const MeasurementSetup& setup) {
		// check binary
		assert_true(boost::filesystem::exists(binary)) << "Invalid executable specified for measurement!";

		// extract name of executable
		const std::string executable = bfs::path(binary).filename().string();

		// partition the papi parameters
		auto papiPartition = partitionPapiCounter(metrics);
		/*auto papiPartition = vector<vector<MetricPtr> >(3);

		namespace idm = insieme::driver::measure;

		papiPartition[0] = vector<MetricPtr>{
			idm::Metric::TOTAL_L1_DCM, idm::Metric::TOTAL_L2_DCM,  idm::Metric::TOTAL_L3_TCM,  idm::Metric::TOTAL_BR_MSP,
			idm::Metric::TOTAL_BR_PRC, idm::Metric::TOTAL_TOT_INS, idm::Metric::TOTAL_FDV_INS, idm::Metric::TOTAL_LD_INS,
		};

		papiPartition[1] = vector<MetricPtr>{
			idm::Metric::TOTAL_FP_INS, idm::Metric::TOTAL_SR_INS, idm::Metric::TOTAL_L2_DCH, idm::Metric::TOTAL_FP_OPS, idm::Metric::TOTAL_VEC_SP,
		};

		papiPartition[2] = vector<MetricPtr>{
			idm::Metric::TOTAL_VEC_DP, idm::Metric::TOTAL_STL_ICY, idm::Metric::TOTAL_TLB_DM, idm::Metric::TOTAL_TLB_IM,
		};*/

		// run experiments and collect results
		vector<std::map<region_id, std::map<MetricPtr, Quantity>>> res;
		for(unsigned i = 0; i < setup.numRuns; i++) {
			// the data to be collected for this run
			Measurements data;

			// run once for each parameter sub-set computed by the partitioning
			for_each(papiPartition, [&](const vector<MetricPtr>& paramList) {

				// create a directory
				int counter = 2;
				auto workdir = bfs::path(".") / ("work_dir_" + executable);
				while(!bfs::create_directory(workdir)) {
					// work directory is already in use => use another one
					workdir = bfs::path(".") / format("work_dir_%s_%d", executable.c_str(), counter++);
				}
				assert_true(bfs::exists(workdir)) << "Working-Directory already present!";

				// setup runtime system metric selection
				std::map<string, string> mod_env = setup.env;
				mod_env["IRT_INST_REGION_INSTRUMENTATION"] = "enabled";

				string metric_selection;
				for(auto metric : getDependencyClosureLeafs(metrics)) {
					// only add non-papi metrics (identified by the PAPI prefix)
					if(metric->getName().find("PAPI") != 0) { metric_selection += metric->getName() + ","; }
				}
				if(!paramList.empty()) { // only set if there are any parameters (otherwise collection is disabled)
					metric_selection += getPapiCounterSelector(paramList);
				} else {
					metric_selection.erase(metric_selection.size() - 1);
				}

				mod_env["IRT_INST_REGION_INSTRUMENTATION_TYPES"] = metric_selection;

				// run code
				int ret = setup.executor->run(binary, mod_env, setup.params, workdir.string());
				if(ret != 0) {
					LOG(WARNING) << "Unexpected executable return code " << ret;
				}

				// load data and merge it
				if(ret == 0) {
					data.mergeIn(loadResults(workdir));
				}

				// delete local files
				if(boost::filesystem::exists(workdir)) { bfs::remove_all(workdir); }

			});

			if(!data.empty()) {
				// extract results
				res.push_back(std::map<region_id, std::map<MetricPtr, Quantity>>());
				auto& curRes = res.back();
				for(const auto& region : data.getAllRegions()) {
				//for_each(data.getAllRegions(), [&](const region_id& region) {
					for(const auto& metric : metrics) {
					//for_each(metrics, [&](const MetricPtr& metric) {
						// use extractor of metric to collect values
						curRes[region][metric] = metric->extract(data, region);
					}//);
				}//);
			}
		}

		// return result
		return res;
	}

	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measurePreinstrumented(const core::NodePtr& root, const vector<MetricPtr>& metrics,
		                                                                              const MeasurementSetup& setup) {
		auto binary = buildBinary(root, setup);
		return measure(binary, metrics, setup);
	}

	std::string buildBinary(const core::NodePtr& root, const MeasurementSetup& setup) {

		// create resulting program
		core::ProgramPtr program;
		if(root->getNodeType() == core::NT_LambdaExpr) {
			program = core::Program::get(root->getNodeManager(), toVector(root.as<core::ExpressionPtr>()));
		} else {
			program = wrapIntoProgram(root);
		}

		// create backend code
		auto backend = backend::runtime::RuntimeBackend::getDefault();
		auto targetCode = backend->convert(program);

		// customize compiler
		utils::compiler::Compiler modifiedCompiler = setup.compiler;

		// add flags required by the runtime
		modifiedCompiler.addFlag(string("-I ") + utils::getInsiemeSourceRootDir() + "runtime/include");
		modifiedCompiler.addFlag(string("-I ") + utils::getInsiemeSourceRootDir() + "common/include");
		modifiedCompiler.addFlag(string("-I ") + utils::getInsiemeLibsRootDir() + "papi-latest/include");
		modifiedCompiler.addFlag(string("-L ") + utils::getInsiemeLibsRootDir() + "papi-latest/lib/");
		modifiedCompiler.addFlag("-D_XOPEN_SOURCE=700 -D_GNU_SOURCE");
		modifiedCompiler.addFlag("-DIRT_ENABLE_REGION_INSTRUMENTATION");
		modifiedCompiler.addFlag("-DIRT_WORKER_SLEEPING");
		modifiedCompiler.addFlag("-DIRT_SCHED_POLICY=IRT_SCHED_POLICY_STATIC");
		#ifdef USE_PAPI
			modifiedCompiler.addFlag("-DIRT_USE_PAPI");
			modifiedCompiler.addFlag(string("-Wl,-rpath,") + utils::getInsiemeLibsRootDir() + "papi-latest/lib -lpapi");
		#endif
		modifiedCompiler.addFlag("-ldl -lrt -lpthread -lm");
		modifiedCompiler.addFlag("-Wno-unused-but-set-variable");
		modifiedCompiler.addFlag("-Wno-unused-variable");

		// compile code to binary
		return utils::compiler::compileToBinary(*targetCode, modifiedCompiler);
	}

	void Measurements::add(worker_id worker, region_id region, const MetricPtr metric, const Quantity& value) {
		workerDataStore[worker][region][metric].push_back(value); // just add value
	}

	void Measurements::add(region_id region, const MetricPtr metric, const Quantity& value) {
		regionDataStore[region][metric].push_back(value);
	}

	void Measurements::mergeIn(const Measurements& other) {
		// a lambda merging region data
		static auto mergeRegionData = [](RegionDataStore& trg, const RegionDataStore& src) {
			for_each(src, [&](const RegionDataStore::value_type& value) {
				region_id region = value.first;
				for_each(value.second, [&](const std::pair<MetricPtr, vector<Quantity>>& inner) {
					vector<Quantity>& list = trg[region][inner.first];
					if(list.empty()) { list.insert(list.end(), inner.second.begin(), inner.second.end()); }
				});
			});
		};

		// merge the region data
		mergeRegionData(regionDataStore, other.regionDataStore);

		// merge the region data structures within the worker data stores
		for_each(other.workerDataStore, [&](const WorkerDataStore::value_type& value) { mergeRegionData(workerDataStore[value.first], value.second); });
	}

	vector<Quantity> Measurements::getAll(region_id region, const MetricPtr metric) const {
		vector<Quantity> res;

		// a lambda merging region data
		auto collectRegionData = [&](const RegionDataStore& src) {
			auto outer_pos = src.find(region);
			if(outer_pos != src.end()) {
				auto pos = outer_pos->second.find(metric);
				if(pos != outer_pos->second.end()) {
					const vector<Quantity>& list = pos->second;
					res.insert(res.end(), list.begin(), list.end());
				}
			}
		};

		// collect data from region store
		collectRegionData(regionDataStore);

		// collect data from worker store
		for_each(workerDataStore, [&](const WorkerDataStore::value_type& value) { collectRegionData(value.second); });

		return res;
	}

	std::set<region_id> Measurements::getAllRegions() const {
		std::set<region_id> res;

		auto collectRegions = [&](const RegionDataStore& regionStore) {
			for_each(regionStore, [&](const RegionDataStore::value_type& cur) { res.insert(cur.first); });
		};

		collectRegions(regionDataStore);

		for_each(workerDataStore, [&](const WorkerDataStore::value_type& value) { collectRegions(value.second); });

		return res;
	}


	namespace {

		vector<string> readLine(std::ifstream& in) {
			typedef boost::tokenizer<boost::escaped_list_separator<char>> Tokenizer;

			string line;
			std::getline(in, line);
			Tokenizer tok(line);
			return vector<string>(tok.begin(), tok.end());
		}

		void loadFile(const boost::filesystem::path& file, const std::function<void(region_id id, const MetricPtr metric, const Quantity& value)>& add) {
			// check whether file exists
			if(!boost::filesystem::exists(file)) { return; }

			// open file
			std::ifstream in(file.string());
			if(!in.is_open()) { return; }

			// ---- Header ----

			// read head line
			vector<string> line = readLine(in);
			if(line.empty() || line[0] != "#subject") {
				LOG(WARNING) << "Invalid file format encountered - no head line - skipping file: " << file;
				return; // skip this file
			}

			// try identifying metrics
			vector<MetricPtr> metrics;
			for(std::size_t i = 2; i < line.size(); i++) {
				auto metric = Metric::getForNameAndUnit(line[i]);
				if(!metric) { LOG(WARNING) << "Unsupported metric encountered - will be ignored: " << line[i]; }
				metrics.push_back(metric);
			}

			// ---- Data ----

			line = readLine(in);
			while(!line.empty()) {
				// make sure line starts with "RG" => rest ignored
				if(line[0] != "RG") {
					line = readLine(in);
					continue;
				}

				// read region id
				region_id region = utils::numeric_cast<region_id>(line[1]);

				// read metrics
				for(std::size_t i = 2; i < line.size(); i++) {
					assert_lt(i - 2, metrics.size()) << "To long row within performance result file!";

					// check metric (skip unknown metrics)
					MetricPtr metric = metrics[i - 2];
					if(!metric) { continue; }

					// convert into value and save within result
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
		const std::string fileBasename = "worker_efficiency.log";

		// try loading a single file in case only a single Insieme runtime was executed
		loadFile(directory / fileBasename, [&](region_id region, const MetricPtr metric, const Quantity& value) { res.add(region, metric, value); });

		// if single file was not present
		if(res.empty()) {
			int index = -1;
			bool stop = false;
			// try checking for multiple files with ID suffix and stop at first unsuccessful file parsing
			while(!stop) {
				stop = true;
				++index;
				string filename = fileBasename + string(".") + std::to_string(index);
				loadFile(directory / filename, [&](region_id region, const MetricPtr metric, const Quantity& value) {
					// add to measurement data, with the index being the worker id
					res.add(index, region, metric, value);
					stop = false;
				});
			}
			LOG(INFO) << "found " << index << " log files";
		}

		return res;
	}


} // end namespace measure
} // end namespace driver
} // end namespace insieme
