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

#include <iostream>
#include <fstream>
#include <sstream>

#include <boost/program_options.hpp>

#include "insieme/core/ir.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/parser/ir_parser.h"
#include <insieme/core/dump/json_dump.h>
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/binary_haskell.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/backend/sequential/sequential_backend.h"

/**
 * This executable is realizing the control flow required for optimizing
 * programs using the Insieme compiler infrastructure.
 */

using namespace std;
using namespace insieme;
using namespace insieme::core;
namespace bpo = boost::program_options;

/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;
	bool statement;
	string inputFile;
	string outFile;
	string dumpFile;
	string dumpJson;
	string binaryDumpFile;
	string haskellBinaryDumpFile;
};

/**
 * Parses command line options for this executable. The given options are
 * parsed and the results are written
 */
CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail = {0};
	fail.valid = false;

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()
		("help,h", "produce help message")
		("version,v", "output version information")
		("statement,s", "parse only a statement instead of a whole program")
		("input,i", bpo::value<string>()->default_value(""), "the code file to be parsed")
		("output,o", bpo::value<string>()->default_value(""), "the binary build from the code")
		("dump-ir,d", bpo::value<string>()->default_value(""), "file to dump the IR to")
		("json-dump-ir,j", bpo::value<string>()->default_value(""), "file to dump the IR to (JSON format)")
		("binary-dump-ir,b", bpo::value<string>()->default_value(""), "file to dump the IR to (binary format)")
		("haskell-binary-dump-ir,k", bpo::value<string>()->default_value(""), "file to dump the IR to (binary format for Haskell)");

	// define positional options (all options not being named)
	bpo::positional_options_description pos;
	pos.add("input", -1);

	// parse parameters
	bpo::variables_map map;
	bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
	bpo::notify(map);

	if(map.count("help")) {
		cout << desc << "\n";
		return fail;
	}

	if(map.count("version")) {
		cout << "Insieme Inspire Parser Version 0.0.3\n";
		return fail;
	}

	if(map["output"].as<string>() == "-") {
		cerr << "Cannot output result to STDOUT\n";
		return fail;
	}

	if(map.count("statement") && !map["output"].as<string>().empty()) {
		cerr << "Cannot compile a single statement\n";
		return fail;
	}

	CmdOptions res = {0};
	res.valid = true;
	res.statement = map.count("statement") == 1;
	res.inputFile = map["input"].as<string>();
	res.outFile = map["output"].as<string>();
	res.dumpFile = map["dump-ir"].as<string>();
	res.dumpJson = map["json-dump-ir"].as<string>();
	res.binaryDumpFile = map["binary-dump-ir"].as<string>();
	res.haskellBinaryDumpFile = map["haskell-binary-dump-ir"].as<string>();

	return res;
}

/**
 * The Insieme Inspire Parser
 */
int main(int argc, char** argv) {
	CmdOptions options = parseCommandLine(argc, argv);
	if(!options.valid) return 1;

	// load input
	stringstream input;
	if(options.inputFile == "-") {
		input << cin.rdbuf();
	} else if(!options.inputFile.empty()) {
		input << fstream(options.inputFile).rdbuf();
	} else {
		cerr << "No input specified\n";
		return 1;
	}
	if(options.inputFile == "") {
		std::cerr << "No input file specified!\n";
		return 1;
	}

	// parse input
	NodeManager manager;
	NodePtr res;
	try {
		if(options.statement) {
			res = core::parser::parseStmt(manager, input.str(), true);
		} else {
			res = core::parser::parseProgram(manager, input.str(), true);
		}
		if(!res) {
			cerr << "Unknown parsing error\n";
			return 1;
		}
	} catch(const core::parser::IRParserException& pe) {
		cerr << "Parsing error encountered: " << pe.what() << "\n";
		return 1;
	}

	// dump IR
	if(options.dumpFile == "-") {
		cout << dumpPretty(res) << endl;
	} else if(!options.dumpFile.empty()) {
		ofstream(options.dumpFile) << dumpPretty(res) << endl;
	}

	// dump IR json
	if(options.dumpJson == "-") {
		dump::json::dumpIR(cout, res);
	} else if(!options.dumpJson.empty()) {
		ofstream out(options.dumpJson);
		dump::json::dumpIR(out, res);
	}

	// binary dump IR
	if(options.binaryDumpFile == "-") {
		dump::binary::dumpIR(cout, res);
	} else if(!options.binaryDumpFile.empty()) {
		ofstream out(options.binaryDumpFile);
		dump::binary::dumpIR(out, res);
	}

	// binary dump IR (Haskell)
	if(options.haskellBinaryDumpFile == "-") {
		dump::binary::haskell::dumpIR(cout, res);
	} else if(!options.haskellBinaryDumpFile.empty()) {
		ofstream out(options.haskellBinaryDumpFile);
		dump::binary::haskell::dumpIR(out, res);
	}

	auto msg = checks::check(res);
	if(msg.size() > 0) {
		cerr << "Encountered issues:\n" << msg << "\n";
		return 1;
	}

	// exit if no output is needed
	if(options.outFile.empty()) return 0;

	// convert to target code and compile code
	auto code = backend::sequential::SequentialBackend().convert(res);

	// pick C / C++ compiler depending on IR
	utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
	compiler = utils::compiler::Compiler::getOptimizedCompiler(compiler);
	if(core::analysis::isIRpp(res)) {
		cerr << "Compiling using C++ compiler ...\n";
		compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler = utils::compiler::Compiler::getOptimizedCompiler(compiler);
	} else {
		cerr << "Compiling using C compiler ...\n";
	}

	bool success = utils::compiler::compileToBinary(*code, options.outFile, compiler);

	return (success) ? 0 : 1;
}
