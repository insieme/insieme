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
		 * A list of name spaces to be intercepted by the frontend.
		 */
		vector<string> interceptedNameSpaces;

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
		 * A flag indicating whether C++11 should be enabled within the frontend or not.
		 */
		bool enableCXX11;

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
		                    const vector<frontend::path>& libDirs, const vector<string>& libNames, const vector<string>& interceptedNameSpaces,
		                    const vector<frontend::path>& interceptedHeaderFileDirectories, bool enableOpenMP, bool enableOpenCL, bool enableCXX11,
		                    const Properties& properties)
		    : name(name), dir(dir), files(files), includeDirs(includeDirs), libDirs(libDirs), libNames(libNames), interceptedNameSpaces(interceptedNameSpaces),
		      interceptedHeaderFileDirectories(interceptedHeaderFileDirectories), enableOpenMP(enableOpenMP), enableOpenCL(enableOpenCL),
		      enableCXX11(enableCXX11), properties(properties) {}

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
		 * Contains the list of namespaces patterns to be intercepted.
		 */
		const vector<string>& getInterceptedNameSpaces() const {
			return interceptedNameSpaces;
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
		 * Determines whether the standard to use is C++11.
		 */
		bool isCXX11() const {
			return enableCXX11;
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
		const map<string, string> getDefinitions(std::string step) const {
			std::map<string, string> defs;
			boost::char_separator<char> sep("\",");
			std::string defStr = properties.get("definitions", step);

			boost::tokenizer<boost::char_separator<char>> tokens = boost::tokenizer<boost::char_separator<char>>(defStr, sep);
			for(const auto& t : tokens) {
				std::string name = t;
				if(!name.empty()) {
					if(name.find("=") != std::string::npos) {
						defs[name.substr(0, name.find("="))] = name.substr(name.find("=") + 1);
					} else {
						defs[name] = "1";
					}
				}
			}
			return defs;
		}


		/**
		 * Obtains a list of additional arguments to be passed on to the compiler when building the test case.
		 */
		const vector<string> getCompilerArguments(std::string step) const {
			// TODO move this to the right place
			// TODO implement properties which exclude each other e.g. use_cpp and use_cpp11
			std::map<string, string> gccFlags;
			gccFlags["use_libmath"] = "-lm";
			gccFlags["use_libpthread"] = "-lpthread";
			gccFlags["use_opencl"] = "-lOpenCL";
			gccFlags["use_omp"] = "-fopenmp";
			gccFlags["standardFlags"] = "-fshow-column -Wall -lrt -pipe";
			gccFlags["use_o3"] = "-O3";
			gccFlags["use_c"] = "--std=c99";
			gccFlags["use_cpp"] = "--std=c++03";
			gccFlags["use_cpp11"] = "--std=c++0x";
			gccFlags["use_gnu99"] = "--std=gnu99";
			gccFlags["use_gnu90"] = "--std=gnu90";

			std::map<string, string> insiemeccFlags;
			insiemeccFlags["use_libmath"] = "";
			insiemeccFlags["use_libpthread"] = "";
			insiemeccFlags["use_opencl"] = "--flib-icl -lOpenCL";
			insiemeccFlags["use_omp"] = "-fopenmp";
			insiemeccFlags["standardFlags"] = "--log-level=INFO";
			insiemeccFlags["use_o3"] = "-O3";
			insiemeccFlags["use_c"] = "";
			insiemeccFlags["use_gnu99"] = "";
			insiemeccFlags["use_gnu90"] = "";
			insiemeccFlags["use_cpp"] = "--std=c++03";
			insiemeccFlags["use_cpp11"] = "--std=c++11";

			std::map<string, map<string, string>> propFlags;
			propFlags["gcc"] = gccFlags;

			gccFlags["standardFlags"] += " -fpermissive";

			propFlags["g++"] = gccFlags;
			propFlags["insiemecc"] = insiemeccFlags;

			boost::filesystem::path comp(properties.get("compiler", step));
			std::string cmd = " ";
			map<std::string, std::string> flagMap = propFlags[comp.filename().string()];

			vector<string> compArgs;
			compArgs.push_back(flagMap["standardFlags"]);

			for(const auto& key : properties.getKeys()) {
				string propVal = properties.get(key, step);
				// check if property is switched on
				if(propVal.compare("1") == 0) {
					// check if property is supported
					if(flagMap.count(key) == 1) {
						compArgs.push_back(flagMap[key]);
					} else {
						std::cout << "WARNING: Property " << key << " not supported!" << std::endl;
					}
				}
				if(propVal.compare("0") == 0 and flagMap.count(key) != 1) { std::cout << "WARNING: Property " << key << " ignored!" << std::endl; }
			}

			// add remaining flags
			compArgs.push_back(properties.get("compFlags", step));

			return compArgs;
		}
	};

	// an optional type wrapping a test case
	typedef boost::optional<IntegrationTestCase> IntegrationTestCaseOpt;

	/**
	 * Obtains a full list of all test cases available within the system.
	 */
	const vector<IntegrationTestCase>& getAllCases(const bool blacklistedOnly = false);

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
