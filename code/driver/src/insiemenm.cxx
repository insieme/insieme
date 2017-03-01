/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
/**
 * Within this file a small, simple example of a compiler driver utilizing
 * the insieme compiler infrastructure is presented.
 *
 * This file is intended to provides a template for implementing new compiler
 * applications utilizing the Insieme compiler and runtime infrastructure.
 */

#include <string>

#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"
#include "insieme/driver/utils/object_file_utils.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/tu/ir_translation_unit.h"


using namespace std;

using namespace std;
using namespace insieme;
using namespace insieme::core;
using namespace insieme::driver;
namespace bpo = boost::program_options;
namespace bfs = boost::filesystem;


namespace {

	void PerformChecks(const core::tu::IRTranslationUnit& tu) {
		unsigned count = 0;
		std::cout << " =======================================================" << std::endl;
		std::cout << "  checking " << tu.getTypes().size() << " types" << std::endl;
		std::cout << " =======================================================" << std::endl;

		for(const auto& pair : tu.getTypes()) {
			if(pair.second.isa<core::TagTypePtr>()) {
				// retrieve metainfo for the object.
				//  those objects should not fail, this test does not really make sense, frontend assertion enforce the right behaviour of this,
				//  it never failed, but i wont remove it since is not very expensive

				// TODO: run semantic checks on member functions of records ...

				// stop on 10
				if(count > 10) {
					std::cout << " ...  only first " << count << " errors shown " << std::endl;
					return;
				}
			}
		}

		if(count != 0) { return; }

		std::cout << " =======================================================" << std::endl;
		std::cout << "  checking: " << tu.getFunctions().size() << " functions" << std::endl;
		std::cout << " =======================================================" << std::endl;
		count = 0;
		for(auto cur : tu.getFunctions()) {
			auto messages = checks::check(cur.second);

			if(!messages.empty()) {
				std::cout << "semantic errors in:  " << cur.first->getStringValue() << std::endl;
				dumpPretty(cur.second);
				for(auto err : messages.getErrors()) {
					std::cout << err << std::endl << std::endl;
					if(count++ > 10) {
						std::cout << " ...  only first " << count << " errors shown " << std::endl;
						return;
					}
				}
			}
		}
	}


} // anonymous namespace


/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;
	bfs::path inputFile;
	bool printDefinitions;
	bool semanticChecks;
};

/**
 * Parses command line options for this executable. The given options are
 * parsed and the results are written
 */
CmdOptions parseCommandLine(int argc, char** argv);


int main(int argc, char** argv) {
	CmdOptions options = parseCommandLine(argc, argv);

	// check options validity
	if(!options.valid) { return 0; }

	// check input file
	if(!driver::utils::isInsiemeLib(options.inputFile)) {
		std::cout << "Not a proper insieme library: " << options.inputFile << "\n";
		return 1;
	}

	// load input file
	NodeManager mgr;
	auto tu = driver::utils::loadLib(mgr, options.inputFile);

	// perform semantic checks (do not print other output)
	if(options.semanticChecks) {
		PerformChecks(tu);
		return 0;
	}

	// print library content
	std::cout << "Contained Symbols:\n";
	std::cout << "    Types:\n";
	for(auto cur : tu.getTypes()) {
		std::cout << "        " << cur.first->getFamilyName();
		if(options.printDefinitions) { std::cout << " : " << *cur.second; }
		std::cout << "\n";
	}

	std::cout << "    Globals:\n";
	for(auto cur : tu.getGlobals()) {
		std::cout << "        " << *cur.first << "\n";
	}

	std::cout << "    Functions:\n";
	for(auto cur : tu.getFunctions()) {
		std::cout << "        " << cur.first->getStringValue();
		if(options.printDefinitions) { std::cout << " : " << *cur.second; }
		std::cout << "\n";
	}

	// print a line of statistical information
	std::cout << tu.getTypes().size() << " types, " << tu.getGlobals().size() << " globals, " << tu.getFunctions().size() << " functions\n";


	// done
	return 0;
}


CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail;
	fail.valid = false;

	// -- parsing -------------------------------------------

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()("help,h", "produce help message")("input,i", bpo::value<bfs::path>(), "the object file to be parsed")(
	    "definitions,d", "enables printing the included definitions")("check", "performs semantic checks in the functions of the tu");

	// define positional options (all options not being named)
	bpo::positional_options_description pos;
	pos.add("input", -1);

	// parse parameters
	bpo::variables_map map;
	bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
	bpo::notify(map);


	// -- processing -----------------------------------------

	// check whether help was requested
	if(map.count("help")) {
		cout << desc << "\n";
		return fail;
	}

	if(!map.count("input")) {
		cout << "Missing input file.\n";
		return fail;
	}

	CmdOptions res;
	res.valid = true;
	res.inputFile = map["input"].as<bfs::path>();
	res.printDefinitions = map.count("definitions");
	res.semanticChecks = map.count("check");

	// accumulation complete
	return res;
}
