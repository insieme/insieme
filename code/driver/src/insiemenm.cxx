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

#include "insieme/driver/object_file_utils.h"

using namespace std;

using namespace std;
using namespace insieme;
using namespace insieme::core;
using namespace insieme::driver;
namespace bpo = boost::program_options;
namespace bfs = boost::filesystem;


/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;
	bfs::path inputFile;
	bool printDefinitions;
};

/**
 * Parses command line options for this executable. The given options are
 * parsed and the results are written
 */
CmdOptions parseCommandLine(int argc, char** argv);


int main(int argc, char** argv) {
	// filter logging messages
	Logger::setLevel(ERROR);

	CmdOptions options = parseCommandLine(argc, argv);

	// check options validity
	if (!options.valid) return 0;

	// check input file
	if (!isInsiemeLib(options.inputFile)) {
		std::cout << "Not a proper insieme library: " << options.inputFile << "\n";
		return 1;
	}

	// load input file
	NodeManager mgr;
	auto tu = loadLib(mgr, options.inputFile);

	// print library content
	std::cout << "Contained Symbols:\n";
	std::cout << "    Types:\n";
	map<string,TypePtr> types;
	for(auto cur : tu.getTypes()) {
		types[cur.first->getFamilyName()] = cur.second;
	}
	for (auto cur : types) {
		std::cout << "        " << cur.first;
		if (options.printDefinitions) std::cout << " : " << *cur.second;
		std::cout << "\n";
	}

//	std::cout << "    Globals:\n";
//	for (auto cur : tu.getGlobals()) {
//		std::cout << "        " << *cur.first << "\n";
//	}

	std::cout << "    Functions:\n";
	map<string,LambdaExprPtr> funs;
	for(auto cur : tu.getFunctions()) {
		funs[cur.first->getStringValue()] = cur.second;
	}
	for (auto cur : funs) {
		std::cout << "        " << cur.first;
		if (options.printDefinitions) std::cout << " : " << *cur.second->getType();
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
	desc.add_options()
			("help,h", 				"produce help message")
			("input,i", 			bpo::value<bfs::path>(), 			"the object file to be parsed")
			("definitions,d", 		"enables printing the included definitions")
	;

	// define positional options (all options not being named)
	bpo::positional_options_description pos;
	pos.add("input", -1);

	// parse parameters
	bpo::variables_map map;
	bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
	bpo::notify(map);


	// -- processing -----------------------------------------

	// check whether help was requested
	if (map.count("help")) {
		cout << desc << "\n";
		return fail;
	}

	if (!map.count("input")) {
		cout << "Missing input file.\n";
		return fail;
	}

	CmdOptions res;
	res.valid = true;
	res.inputFile = map["input"].as<bfs::path>();
	res.printDefinitions = map.count("definitions");

	// accumulation complete
	return res;
}
