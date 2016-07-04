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

#include <map>
#include <set>
#include <vector>
#include <string>

#include <boost/utility.hpp>
#include <boost/filesystem/path.hpp>

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/driver/measure/quantity.h"
#include "insieme/driver/measure/executor.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace driver {
namespace measure {

	class Metric;
	typedef const Metric* MetricPtr;

	struct MeasurementSetup;

	// a type definition for the type used to index regions
	typedef unsigned region_id;


	// --------------------------------------------------------------------------------------------
	//										Main Utilities
	// --------------------------------------------------------------------------------------------

	/**
	* A utility function that returns a default compiler setup for measurements
	*/
	utils::compiler::Compiler getDefaultCompilerForMeasurments();

	MeasurementSetup getDefaultMeasurementSetup();

	/**
	 * A utility struct that holds measurement setup, environment and parameters
	 */
	struct MeasurementSetup : public insieme::utils::Printable {
		backend::runtime::RuntimeBackendPtr backend;
		utils::compiler::Compiler compiler;
		ExecutorPtr executor;
		unsigned numRuns;
		std::map<std::string, std::string> env;
		std::vector<std::string> params;

		MeasurementSetup()
			: backend(backend::runtime::RuntimeBackend::getDefault()), compiler(getDefaultCompilerForMeasurments()),
			  executor(std::make_shared<LocalExecutor>()), numRuns(1), env(), params() {
		}

		MeasurementSetup withCompiler(const utils::compiler::Compiler newCompiler) const {
			MeasurementSetup retVal = *this;
			retVal.compiler = newCompiler;
			return retVal;
		}

		MeasurementSetup withExecutor(const ExecutorPtr newExecutor) const {
			MeasurementSetup retVal = *this;
			retVal.executor = newExecutor;
			return retVal;
		}

		MeasurementSetup withNumRuns(const unsigned newNumRuns) const {
			MeasurementSetup retVal = *this;
			retVal.numRuns = newNumRuns;
			return retVal;
		}

		MeasurementSetup withEnvironment(const std::map<std::string, std::string> newEnv) const {
			MeasurementSetup retVal = *this;
			retVal.env = newEnv;
			return retVal;
		}

		MeasurementSetup withParameters(const std::vector<std::string> newParams) const {
			MeasurementSetup retVal = *this;
			retVal.params = newParams;
			return retVal;
		}

		std::ostream& printTo(std::ostream& out) const {
			out << "MeasurementSetup:\n";
			out << "\tCompiler: " << compiler << "\n";
			out << "\tExecutor: " << *executor << "\n";
			out << "\tnumRuns: " << numRuns << "\n";
			out << "\tenv: " << env << "\n";
			out << "\tparams: " << params << "\n";
			return out;
		}

	};

	/**
	 * Measures a single metric for a single statement for a given number of times.
	 *
	 * @param stmt the statement to be measured
	 * @param metric the metric to be collected
	 * @param setup the measurement setup by which to conduct the measurement
	 * @return the list of measured quantities containing one entry per code execution
	 */
	vector<Quantity> measure(const core::StatementPtr& stmt, const MetricPtr& metric, const MeasurementSetup& setup);

	/**
	 * Measures a single metric for a single statement for a given number of times.
	 *
	 * @param stmt the statement to be measured
	 * @param metric the metric to be collected
	 * @param setup the measurement setup by which to conduct the measurement
	 * @return the list of measured quantities containing one entry per code execution
	 */
	vector<Quantity> measure(const core::StatementAddress& stmt, const MetricPtr& metric, const MeasurementSetup& setup);

	/**
	 * Measures a single metric for a single statement within a code fragment using
	 * the given executor.
	 *
	 * @param stmt the statement to be converted into a binary and executed.
	 * @param metrics the metrics to be collected
	 * @param setup the measurement setup by which to conduct the measurement
	 * @return the measured quantity
	 */
	vector<std::map<MetricPtr, Quantity>> measure(const core::StatementAddress& stmt, const vector<MetricPtr>& metrics, const MeasurementSetup& setup);

	/**
	 * Measures a list of metrics for a list of regions within a single program for a given number of times.
	 *
	 * @param regions the regions to be measured. They are not instrumented yet, but will be instrumented
	 * 			using the given region IDs. If two addresses have the same region id assigned, their results
	 * 			will be aggregated
	 * @param metrics the metrics to be collected
	 * @param setup the measurement setup by which to conduct the measurement
	 * @return a vector containing the results of each individual run. Each result is mapping regions the collected
	 * 		values data indexed by the requested metrics.
	 */
	vector<std::map<core::StatementAddress, std::map<MetricPtr, Quantity>>> measure(const vector<core::StatementAddress>& regions,
		                                                                            const vector<MetricPtr>& metrices, const MeasurementSetup& setup);

	/**
	 * Measures a list of metrics for a list of regions within a single program for a given number of times.
	 *
	 * @param regions the regions to be measured. They are not instrumented yet, but will be instrumented
	 * 			using the given region IDs. If two addresses have the same region id assigned, their results
	 * 			will be aggregated
	 * @param metrics the metrics to be collected
	 * @param setup the measurement setup by which to conduct the measurement
	 * @return a vector containing the results of each individual run. Each result is mapping regions the collected
	 * 		values data indexed by the requested metrics.
	 */
	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(const std::map<core::StatementAddress, region_id>& regions,
		                                                               const vector<MetricPtr>& metrices, const MeasurementSetup& setup);

	/**
	 * Measures a list of metrics for a binary for a given number of times.
	 *
	 * @param binary the name / path to the binary
	 * @param metrics the metrics to be collected
	 * @param setup the measurement setup by which to conduct the measurement
	 * @return a vector containing the results of each individual run. Each result is mapping regions the collected
	 * 		values data indexed by the requested metrics.
	 */
	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measure(const std::string& binary, const vector<MetricPtr>& metrices,
		                                                               const MeasurementSetup& setup);

	/**
	* Measures a list of metrics for an already instrumented IR for a given number of times.
	*
	* @param root the root of the already instrumented IR to be measured
	* @param metrics the metrics to be collected
	* @param setup the measurement setup by which to conduct the measurement
	* @return a vector containing the results of each individual run. Each result is mapping regions the collected
	* 		values data indexed by the requested metrics.
	*/
	vector<std::map<region_id, std::map<MetricPtr, Quantity>>> measurePreinstrumented(const core::NodePtr& root, const vector<MetricPtr>& metrics,
		                                                                              const MeasurementSetup& setup);


	// --------------------------------------------------------------------------------------------
	//										Building
	// --------------------------------------------------------------------------------------------

	/**
	 * Creates an instrumented binary based on the given regions using the given compiler.
	 *
	 * @param the root of the IR to be built
	 * @param compiler the compiler to be used to build the resulting binary. If the build fails,
	 * 			an empty string will be returned.
	 * @return the path to the produced binary
	 */
	std::string buildBinary(const core::NodePtr& root, const MeasurementSetup& setup);


	// --------------------------------------------------------------------------------------------
	//										Metric
	// --------------------------------------------------------------------------------------------


	// forward declaration
	class Measurements;

	/**
	 * A class used to model a metric within the measuring infrastructure.
	 */
	class Metric : public utils::Printable, public boost::noncopyable {
	  public:
		/**
		 * The type of function to be used for extracting values form measurements.
		 */
		typedef std::function<Quantity(const Measurements& data, MetricPtr, region_id)> extractor_type;

	  private:
		/**
		 * The name of this metric. This name is a unique identifier for
		 * a metric.
		 */
		string name;

		/**
		 * The unit of the measurement produced by this metric.
		 */
		UnitPtr unit;

		/**
		 * The extractor used to obtain this metric from some measurement data.
		 * Beside extracting the information, it is also aggregating it.
		 */
		extractor_type extractor;

		/**
		 * A set referencing all the metrics this metric is depending on.
		 */
		std::set<MetricPtr> dependencies;

	  public:
		/**
		 * Create a new, derived metric instance.
		 *
		 * @param name the name of the new metric
		 * @param unit the unit of the resulting quantity
		 * @param extractor the function to be used for extracting this metric
		 * 		from some measurements
		 * @param dependencies the set of metrics this new metric is derived from
		 */
		Metric(const string& name, const UnitPtr& unit, const extractor_type& extractor, const std::set<MetricPtr>& dependencies)
		    : name(name), unit(unit), extractor(extractor), dependencies(dependencies){};

		/**
		 * Obtains a reference to the name of this metric.
		 */
		const string& getName() const {
			return name;
		}

		/**
		 * Obtains a reference to the unit used by this metric.
		 */
		const UnitPtr& getUnit() const {
			return unit;
		}

		/**
		 * Obtains a reference to the internally maintained set of metrics this
		 * metric is depending on.
		 */
		const std::set<MetricPtr>& getDependencies() const {
			return dependencies;
		}

		/**
		 * Extracts the quantity of this metric from the given data.
		 * If the necessary information is not available, the resulting
		 * quantity will be invalid.
		 *
		 * @param data the measurements from which this metric should be obtained
		 * @param region the ID of the region for which the data should be extracted
		 * @return the value of this metric within the given data
		 */
		Quantity extract(const Measurements& data, region_id region) const {
			return extractor(data, this, region);
		}

		/**
		 * Enables metrics to be printed to streams.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

		// ------------ Predefined Metrics -------------

		// an enumeration of all supported metrics
		#define METRIC(LITERAL, NAME, UNIT, FUN) static const MetricPtr LITERAL;
		#include "insieme/driver/measure/metrics.def"
		#undef METRIC

		// ---------------------------------------------

		/**
		 * This method will try to obtain a pointer to the metric with the given name and the unit.
		 * If no such metric can be found, a NULL pointer will be returned.
		 * The search will only cover predefined metrics.
		 *
		 * @param name the name and the unit of the metric to be obtained
		 * @return a pointer to the requested metric or NULL in case there is no such metric
		 */
		static const MetricPtr getForNameAndUnit(const string& name);

		/**
		 * This method is obtaining a reference to a vector of metrics containing all
		 * metrics offered as a static constant by this class.
		 */
		static const vector<MetricPtr>& getAll();
	};


	/**
	 * Computes all leaf-metrics the metrics within the given set of metrics are depending on.
	 */
	std::set<MetricPtr> getDependencyClosureLeafs(const std::vector<MetricPtr>& metrics);


	// --------------------------------------------------------------------------------------------
	//										Data Collection
	// --------------------------------------------------------------------------------------------

	// the type used to index worker
	typedef unsigned worker_id;

	/**
	 * This data container is aggregating all the data loaded from the performance log of a test run.
	 */
	class Measurements : public utils::Printable {
		/**
		 * The data structure used to store the data collected per region.
		 */
		typedef std::map<region_id, std::map<MetricPtr, vector<Quantity>>> RegionDataStore;

		/**
		 * The data structure used to store the data collected per worker.
		 */
		typedef std::map<worker_id, RegionDataStore> WorkerDataStore;

		/**
		 * The store for the performance data.
		 */
		RegionDataStore regionDataStore;

		/**
		 * The store for the efficiency data.
		 */
		WorkerDataStore workerDataStore;

	  public:
		/**
		 * Adds a new value to the data collected per worker.
		 *
		 * @param worker the worker producing it
		 * @param region the region the value is attached to
		 * @param metric the metric the value is describing
		 * @param value the value to be attached
		 */
		void add(worker_id worker, region_id region, MetricPtr metric, const Quantity& value);

		/**
		 * Adds a new value to the data collected per region.
		 *
		 * @param region the region the value should be attached to
		 * @param metric the metric the value is describing
		 * @param value the value to be attached
		 */
		void add(region_id region, MetricPtr metric, const Quantity& value);

		/**
		 * Merges all the values stored within the given measurement result into this container.
		 */
		void mergeIn(const Measurements& other);

		/**
		 * Obtains a list of all values values associated to the
		 * given region / metric combination.
		 */
		vector<Quantity> getAll(region_id region, MetricPtr metric) const;

		/**
		 * Obtains a list of all regions referenced within this measurement collection.
		 */
		std::set<region_id> getAllRegions() const;

		/**
		 * Tests whether this container is empty or not.
		 */
		bool empty() const {
			return regionDataStore.empty() && workerDataStore.empty();
		}

		/**
		 * Enables printing the full measurement container.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "Measurements(" << regionDataStore << "," << workerDataStore << ")";
		}
	};


	/**
	 * A utility function loading measurement data from the list of files produced by the runtime profiler.
	 */
	Measurements loadResults(const boost::filesystem::path& directory);


} // end namespace measure
} // end namespace driver
} // end namespace insieme

namespace std {

	// print pointers to metrics
	inline std::ostream& operator<<(std::ostream& out, const insieme::driver::measure::MetricPtr& metric) {
		return out << *metric;
	}
}
