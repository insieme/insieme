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
#include <fstream>

#include <boost/program_options.hpp>

#include "insieme/frontend/frontend.h"

#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/analysis/cba/common/preprocessing.h"

#include "insieme/utils/name_mangling.h"

#include "insieme/driver/integration/test_framework.h"

using namespace std;
using namespace insieme;
namespace fe = insieme::frontend;
namespace opts = boost::program_options;
namespace itc = insieme::driver::integration;

struct CmdOptions {
	bool valid;
	string inputFile;
	string testCase;
	string dumpBinaryHaskell;
	string dumpJson;
};

class CBAInputTestExt : public core::lang::Extension {
	/**
	 * Allow the node manager to create instances of this class.
	 */
	friend class core::NodeManager;

	/**
	 * Creates a new instance based on the given node manager.
	 */
	CBAInputTestExt(core::NodeManager& manager) : core::lang::Extension(manager) {}
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
		("case,c", opts::value<string>()->default_value(""), "the test case to be loaded")
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
	res.testCase = map["case"].as<string>();
	res.dumpBinaryHaskell = map["dump-irbh"].as<string>();
	res.dumpJson = map["dump-json"].as<string>();


	if(res.inputFile=="" && res.testCase=="") {
		cout << "No input file or test case name provided.\n";
		return fail;
	}

	if(res.inputFile!="" && res.testCase!="") {
		cout << "Can only process a given input file or a test case, but not both.\n";
		return fail;
	}


	return res;
}

int main(int argc, char** argv) {
	CmdOptions options = parseCommandLine(argc, argv);
	if(!options.valid) return 1;

	// load the input file
	core::NodeManager mgr;
	core::ProgramPtr program;
	if (!options.testCase.empty()) {
		auto cases = itc::getCase(options.testCase);
		if (!cases) {
			std::cout << "Test case not found: " << options.inputFile << "\n";
			return 1;
		}
		program = cases->load(mgr);
	} else {
		fe::ConversionJob job;
		job.addFile(options.inputFile);
		program = job.execute(mgr);
	}

	// run pre-processing
	program = analysis::cba::preProcessing(program);


	vector<core::NodeAddress> targets;
	core::NodePtr ref_deref = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefDeref();
	core::NodePtr ref_assign = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefAssign();

	core::visitDepthFirst(core::NodeAddress(program), [&](const core::CallExprAddress& call) {
		auto fun = call->getFunctionExpr();
		if(*fun == *ref_deref || *fun == *ref_assign) {
			targets.push_back(call[0]);
		}
	});

	if(options.dumpBinaryHaskell == "-") {
		core::dump::binary::haskell::dumpAddresses(cout, targets);
	} else if(!options.dumpBinaryHaskell.empty()) {
		ofstream out(options.dumpBinaryHaskell);
		core::dump::binary::haskell::dumpAddresses(out, targets);
	}

	if(options.dumpJson == "-") {
		core::dump::json::dumpIR(cout, program);
	} else if(!options.dumpJson.empty()) {
		ofstream out(options.dumpJson);
		core::dump::json::dumpIR(out, program);
	}

	return 0;
}
