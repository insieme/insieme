/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "boost/lexical_cast.hpp"

#include "insieme/backend/backend.h"
#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/driver/measure/executor.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace measure {


	using std::string;
	using std::map;

	/**
	 * Wrapper utility that allows getting system information about hardware or the OS, by using the IRT_REPORT feature
	 */
	class SystemInfo {
	  private:
		/**
		 * if this is false, any queries besides isValid() can cause unspecified behavior
		 */
		bool valid;

		/**
		 * output from IRT_REPORT, to be used for data lookups
		 */
		mutable string data;

		/**
		 * environment variables
		 */
		map<string, string> env;

		/**
		 * defines that need to be set when compiling the runtime
		 */
		map<string, string> backendCompilerDefs;

		/**
		 * backend in use, must be runtime backend for now
		 */
		insieme::backend::BackendPtr backend;

		/**
		 * backend compiler in use, must be runtime compiler for now
		 */
		insieme::utils::compiler::Compiler compiler;

		/**
		 * executor to run the compiled binary, indirection allows to run queries on remote systems
		 */
		ExecutorPtr executor;

		/**
		 * internal function that performs the actual compilation, execution and report saving
		 */
		virtual int obtainSystemInformation() const;

	  public:
		/**
		 * Constructor, initializes data members, triggers the compilation and execution of the test binary and saves the output
		 */
		SystemInfo(map<string, string> env = {{"IRT_REPORT", "1"}, {"IRT_REPORT_TO_FILE", "1"}},
		           map<string, string> backendCompilerDefs = {{"IRT_ENABLE_REGION_INSTRUMENTATION", "1"}, {"IRT_USE_PAPI", "1"}},
		           insieme::backend::BackendPtr backend = insieme::backend::runtime::RuntimeBackend::getDefault(),
		           insieme::utils::compiler::Compiler compiler = insieme::utils::compiler::Compiler::getRuntimeCompiler(),
		           insieme::driver::measure::ExecutorPtr executor = insieme::driver::measure::makeLocalExecutor())
		    : valid(false), data(""), env(env), backendCompilerDefs(backendCompilerDefs), backend(backend), compiler(compiler), executor(executor) {
		    	#ifndef USE_PAPI
				assert_fail() << "SystemInfo requires PAPI";
			#endif
			assert_true(backend) << "SystemInfo requires a valid backend!";
			assert_true(executor) << "SystemInfo requires a valid executor!";

			if(obtainSystemInformation() < 0) {
				valid = false;
				LOG(ERROR) << "Failed trying to obtain system information";
			} else {
				valid = true;
			}
		}

		virtual ~SystemInfo(){};

		/**
		 * Provides a validity check. If this method returns false, all other method calls to this object are invalid.
		 * @return true if the data is valid and queries can be performed, false otherwise
		 */
		virtual bool isValid() const;

		/**
		 * Preset function that allows to query for the number of logical processors present in the target system.
		 * @return the number of logical CPUs
		 */
		virtual unsigned getNumberOfCPUsTotal() const;

		/**
		 * Preset function that allows to query for the number of cores per socket present in the target system.
		 * @return the number of cores per socket
		 */
		virtual unsigned getNumberOfCoresPerSocket() const;

		/**
		 * Preset function that allows to query for the number of sockets present in the target system.
		 * @return the number of sockets
		 */
		virtual unsigned getNumberOfSockets() const;

		/**
		 * Preset function that allows to query for the number of hardware threads per core present in the target system.
		 * E.g. for Intel HyperThreading enabled systems, this returns 2.
		 * @return the number of hardware threads per core
		 */
		virtual unsigned getNumberOfHWThreadsPerCore() const;

		/**
		 * Preset function that allows to check whether cores have their own DVFS domain, i.e. can be clocked individually
		 * @return true if cores have their own DVFS domain, false otherwise
		 */
		virtual bool hasDVFSDomainPerCore() const;

		/**
		 * Allows generic queries by providing a regular expression that uses capturing groups.
		 * @param regex the regular expression with capturing groups to be used when searching for the data in question
		 * @return a vector of strings containing all matches of capturing groups
		 */
		virtual vector<string> queryGenericString(const std::string& regex) const;

		/**
		 * Allows generic queries by providing a regular expression that uses capturing groups. Returns the first match only.
		 * @param regex the regular expression with capturing groups to be used when searching for the data in question
		 * @return a string containing the first match
		 */
		virtual string queryGenericStringSingle(const std::string& regex) const;

		/**
		 * Generic function that returns a vector of typed data holding all matches
		 * @tparam the target type to cast the intermediate result into
		 * @param regex the regular expression with capturing groups to be used when searching for the data in question
		 * @return a vector of specified type containing all matches of capturing groups
		 */
		template <typename T>
		vector<T> queryGeneric(const string& regex) const {
			const vector<string> temp = queryGenericString(regex);
			vector<T> res;
			for(const auto& e : temp) {
				try {
					res.push_back(boost::lexical_cast<T>(e));
				} catch(boost::bad_lexical_cast& blc) { LOG(ERROR) << "Unable to lexically cast " << e << ": " << blc.what() << "\n"; }
			}
			return res;
		}

		/**
		 * Generic function that returns typed data for the first match only
		 * @tparam the target type to cast the intermediate result into
		 * @param regex the regular expression with capturing groups to be used when searching for the data in question
		 * @return data of specified type holding the first match
		 */
		template <typename T>
		T queryGenericSingle(const string& regex) const {
			const auto temp = queryGeneric<T>(regex);
			T res;
			if(!temp.empty()) { res = temp.front(); }
			return res;
		}

		/**
		 * allows more complicated lookups by providing the entire output IRT_REPORT
		 * @return a reference to the full string output of IRT_REPORT
		 */
		virtual const string& getFullInformation() const;
	};

} // end namespace measure
} // end namespace driver
} // end namespace insieme
