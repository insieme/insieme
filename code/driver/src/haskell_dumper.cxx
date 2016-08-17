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

#include <fstream>

#include <boost/program_options.hpp>

#include "insieme/frontend/frontend.h"

#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/dump/json_dump.h"

using namespace std;
namespace co = insieme::core;
namespace fe = insieme::frontend;
namespace opts = boost::program_options;

struct CmdOptions {
	bool valid;
	string inputFile;
	string dumpBinaryHaskell;
	string dumpJson;
};

CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail = {0};
	fail.valid = false;

	// define options
	opts::options_description desc("Supported Parameters");
	desc.add_options()
		("help,h", "produce help message")
		("version,v", "output version information")
		("input,i", opts::value<string>()->default_value(""), "the code file to be parsed")
		("dump-irbh,d", opts::value<string>()->default_value(""), "file to dump IR to (Haskell)")
		("dump-json,j", opts::value<string>()->default_value(""), "file to dump IR to (JSON)");

	opts::positional_options_description pos;
	pos.add("input", -1);

	// parse parameters
	opts::variables_map map;
	opts::store(opts::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
	opts::notify(map);

	if(map.count("help")) {
		cout << desc << "\n";
		return fail;
	}

	if(map.count("version")) {
		cout << "Insieme Haskell Dumper Version 0.0.1\n";
		return fail;
	}

	CmdOptions res = {0};
	res.valid = true;
	res.inputFile = map["input"].as<string>();
	res.dumpBinaryHaskell = map["dump-irbh"].as<string>();
	res.dumpJson = map["dump-json"].as<string>();

	return res;
}

int main(int argc, char** argv) {
	CmdOptions options = parseCommandLine(argc, argv);
	if(!options.valid) return 1;

	// setup frontend
	fe::ConversionJob job;
	job.setFiles({options.inputFile});

	// parse input code
	co::NodeManager mgr;
	auto program = job.execute(mgr);

	if(!options.dumpBinaryHaskell.empty()) {
		ofstream out(options.dumpBinaryHaskell);
		co::dump::binary::haskell::dumpIR(out, program);
	}

	if(!options.dumpJson.empty()) {
		ofstream out(options.dumpJson);
		co::dump::json::dumpIR(out, program);
	}

	return 0;
}
