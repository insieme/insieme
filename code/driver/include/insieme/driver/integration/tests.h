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

#include <string>
#include <map>
#include <vector>
#include <boost/optional.hpp>
#include <boost/filesystem.hpp>
#include <boost/tokenizer.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/config.h"

#include "insieme/core/ir_program.h"
#include "insieme/frontend/frontend.h"

#include "insieme/driver/integration/properties.h"

namespace insieme {
namespace core {
namespace tu {
	class IRTranslationUnit;
}
}
namespace driver {
namespace cmd {
	struct Options;
}
namespace integration {

	using std::map;
	using std::string;
	using std::vector;

	/**
	 * Instances of this class represent integration test cases within
	 */
	class IntegrationTestCase : public utils::Printable {
		/**
		 * The name of this test case.
		 */
		string name;

		/**
		 * The directory forming containing the test case.
		 */
		frontend::path dir;

		/**
		 * The input files of this test case.
		 */
		vector<frontend::path> files;

		/**
		 * Extra include directories to be considered.
		 */
		vector<frontend::path> includeDirs;

		/**
		 * A list of external libraries, paths
		 */
		vector<frontend::path> libDirs;

		/**
		 * A list of external libraries, names
		 */
		vector<string> libNames;

		/**
		 * A list of directories containing header filed to be intercepted.
		 */
		vector<frontend::path> interceptedHeaderFileDirectories;

		/**
		 * A flag indicating whether OpenMP should be enabled within the frontend or not.
		 */
		bool enableOpenMP;

		/**
		 * A flag indicating whether OpenCL should be enabled within the frontend or not.
		 */
		bool enableOpenCL;

		/**
		 * The properties configured for this test case.
		 */
		Properties properties;


	  private:
		friend class boost::serialization::access;
		template <class Archive>
		void serialize(Archive& ar, const unsigned int version) {
			ar& name;
		}

	  public:
		IntegrationTestCase(){};
		/**
		 * Creates a new test case based on the given arguments.
		 */
		IntegrationTestCase(const string& name, const frontend::path& dir, const vector<frontend::path>& files, const vector<frontend::path>& includeDirs,
		                    const vector<frontend::path>& libDirs, const vector<string>& libNames,
		                    const vector<frontend::path>& interceptedHeaderFileDirectories, bool enableOpenMP, bool enableOpenCL,
		                    const Properties& properties)
		    : name(name), dir(dir), files(files), includeDirs(includeDirs), libDirs(libDirs), libNames(libNames),
		      interceptedHeaderFileDirectories(interceptedHeaderFileDirectories), enableOpenMP(enableOpenMP), enableOpenCL(enableOpenCL),
		      properties(properties) {
			if(enableOpenCL) {
				// add the OpenCL specific directories
				this->includeDirs.push_back(utils::getInsiemeLibsRootDir() + "opencl/include");
				this->libDirs.push_back(utils::getInsiemeLibsRootDir() + "opencl/lib64/");
			}
		}

		/**
		 * Obtains the name of this test case.
		 */
		const string& getName() const {
			return name;
		}

		/**
		 * Get Basename of this test case
		 */
		string getBaseName() const {
			return boost::filesystem::path(dir).filename().string();
		}

		/**
		 * Obtains the directory forming this test case.
		 */
		const frontend::path& getDirectory() const {
			return dir;
		}

		/**
		 * Obtains the files this test case is consisting of.
		 */
		const vector<frontend::path>& getFiles() const {
			return files;
		}

		/**
		 * Obtains the list of include directories.
		 */
		const vector<frontend::path>& getIncludeDirs() const {
			return includeDirs;
		}

		/**
		 * Obtains the list of external library directories.
		 */
		const vector<frontend::path>& getLibDirs() const {
			return libDirs;
		}

		/**
		 * Obtains the list of external libs.
		 */
		const vector<string>& getLibNames() const {
			return libNames;
		}

		/**
		 * Obtains the list of directories containing header filed to be intercepted.
		 */
		const vector<frontend::path>& getInterceptedHeaderFileDirectories() const {
			return interceptedHeaderFileDirectories;
		}

		/**
		 * Determines whether the OpenMP conversion should be enabled within the frontend or not.
		 */
		bool isEnableOpenMP() const {
			return enableOpenMP;
		}

		/**
		 * Determines whether the OpenCL conversion should be enabled within the frontend or not.
		 */
		bool isEnableOpenCL() const {
			return enableOpenCL;
		}

		/**
		 * Get the properties configured for this test case.
		 */
		const Properties& getProperties() const {
			return properties;
		}

		/**
		 * Obtains a view on the internally maintained properties for the given
		 * integration test step.
		 */
		PropertyView getPropertiesFor(const string& step) const {
			return properties.getView(step);
		}

		/**
		 * Allows to print this test case to some stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

		/**
		 * An equality operator for integration test case instances.
		 */
		bool operator==(const IntegrationTestCase& other) const {
			return name == other.name;
		}

		/**
		 * A comparison operator for integration test case instances.
		 */
		bool operator<(const IntegrationTestCase& other) const {
			return name < other.name;
		}

		/**
		 * A method that gives an options object that is configured and can be
		 * passed to e.g., conversion job. The options object does not contain
		 * any files that may be used as input sources for this test case.
		 */
		driver::cmd::Options getOptions() const;

		/**
		 * Loads this test case.
		 */
		core::ProgramPtr load(core::NodeManager& manager) const;

		/**
		 * Loads the translation unit for this test case.
		 */
		core::tu::IRTranslationUnit loadTU(core::NodeManager& manager) const;

		/**
		 * Obtains the list of macro definitions to be passed on the the frontend.
		*/
		const map<string, string> getDefinitions(std::string step) const;

		/**
		 * Obtains a list of additional arguments to be passed on to the compiler when building the test case.
		 */
		const vector<string> getCompilerArguments(std::string step, bool considerEnvVars, bool isCpp) const;
	};

	// an optional type wrapping a test case
	typedef boost::optional<IntegrationTestCase> IntegrationTestCaseOpt;

	// an enum describing the different modes which test cases to load
	enum LoadTestCaseMode { ENABLED_TESTS, ENABLED_AND_LONG_TESTS, LONG_TESTS, BLACKLISTED_TESTS, ALL_TESTS };

	/** returns the name/path to the backend compiler to use depending on the passed language and compiler name/path.
	 * This function will give precedence to the INSIEME_C(XX)_BACKEND_COMPILER environment variables
	 */
	std::string getBackendCompilerString(const string& compilerProperty, bool isCpp);

	/**
	 * Obtains a full list of all test cases available within the system.
	 */
	const vector<IntegrationTestCase>& getAllCases(const LoadTestCaseMode loadTestCaseMode = ENABLED_TESTS);

	/**
	 * Obtains the test case matching the given name.
	 *
	 * @param name the name of the test case looking for
	 * @return an optional representing the test case or being uninitialized if there is no such test case.
	 */
	const IntegrationTestCaseOpt getCase(const string& name);

	/**
	 * Obtains a list of test cases in the given path or below.
	 *
	 * @param path the directory representing the integration test
	 * @return the list of test cases within this directory or below
	 */
	vector<IntegrationTestCase> getTestSuite(const string& path);

	/**
	 * This function is loading the integration test with the given name.
	 *
	 * @param manager the manager to be used to load the specified test program
	 * @param name the integration test to be loaded
	 * @param enableOpenMP a flag allowing to enable / disable the OpenMP conversion
	 * @param definitions the list of definitions to be passed to the pre-processor
	 * @return the loaded program
	 */
	core::ProgramPtr loadIntegrationTest(core::NodeManager& manager, const std::string& name, bool enableOpenMP = true,
	                                     const std::map<string, string>& definitions = std::map<string, string>());

	/**
	 * Allow Integration Tests to be properly printed within gtest.
	 */
	inline std::ostream& operator<<(std::ostream& out, const IntegrationTestCase& test) {
		return test.printTo(out);
	}

} // end namespace integration
} // end namespace driver
} // end namespace insieme
