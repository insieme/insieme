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
 */

#include <iostream>
#include <string>
#include <fstream>

#include "insieme/analysis/cba/datalog/facts_to_files_extractor.h"

#include "insieme/core/printer/error_printer.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/ir_node.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/datalog/options.h"

#include "insieme/utils/name_mangling.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>


using namespace std;
using namespace insieme;
using namespace insieme::core;

using insieme::driver::datalog::Options;
using CompilerOptions = insieme::driver::cmd::Options;

using insieme::analysis::cba::datalog::TargetRelations;
using insieme::analysis::cba::datalog::TargetRelationEntry;
using insieme::analysis::cba::datalog::extractPointerFactsToFiles;


int main(int argc, char** argv) {
	// Parse input parameters
	Options args;
	if (!args.parseCommandLine(argc, argv)) {
		cerr << "Could not parse command-line arguments!" << endl;
		return 1;
	}
	if (!args.checkValidity()) {
		return 1;
	}

	// Parse input file and extract facts
	if (args.verboseOutput)
		cout << "Loading file " << args.inputFile << "..." << endl;

	NodeManager mgr;
	std::vector<string> compilerArgv = {"compiler", args.inputFile };
	CompilerOptions compilerOptions = CompilerOptions::parse(compilerArgv);

	auto program = compilerOptions.job.execute(mgr);

	// Semantic checks
	auto res = checks::check(program);
	if (!res.empty()) {
		cerr << "Semantic checks failed!" << endl;
		cerr << printer::dumpErrors(res) << endl;
		return 1;
	}

	// Run though the IR and collect "capture functions"
	TargetRelations targets;

	visitDepthFirst(NodeAddress(program), [&](const CallExprAddress &call) {

		// Only interested in specific calls
		auto fun = call->getFunctionExpr();
		if (!fun.isa<LambdaExprPtr>()) return;

		// Get function name
		const string &mangledName = fun.as<LambdaExprPtr>()->getReference()->getNameAsString();
		const string &name = utils::demangle(mangledName);

		// "Capture functions" have special prefixes
		if (!boost::starts_with(name, "datalog_")) return;

		if (args.verboseOutput)
			cout << "Found function " << name << "...." << endl;

		if (name == "datalog_capture_") {
			auto line = call.getArgument(0).as<CallExprAddress>()->getArgument(0).as<LiteralAddress>()->getStringValue();
			auto varname = call.getArgument(1).as<CallExprAddress>()->getArgument(0).as<LiteralAddress>()->getStringValue();
			auto relation = call.getArgument(2).as<CallExprAddress>()->getArgument(0).as<LiteralAddress>()->getStringValue();
			auto var = call.getArgument(3).as<CallExprAddress>()->getArgument(0).as<CallExprAddress>()->getArgument(0).as<VariableAddress>();

			// remove ticks (") around relation name and var name
			if (relation.at(0) == '"')
				relation = string(relation, 1, relation.size() - 2);
			if (varname.at(0) == '"')
				varname = string(varname, 1, varname.size() - 2);

			if (args.verboseOutput) {
				cout << "  In line " << line << ", found rel "
				     << relation << ", var "
				     << varname << ": " << var << endl;
			}

			if (relation.size() == 0) {
				cerr << "Target with empty relation name in line " << line
				     << ", skipping..." << endl;
			} else {
				TargetRelationEntry entry(var);
				entry.addInfo("VarName" , varname)
				     .addInfo("LineNum" , line)
				     .addInfo("IRVarName", var.toString());

				auto res = targets[relation].insert(entry);

				if (res.second == false) {
					cout << "Ignoring capture of " << varname << " in line " << line
					     << " because it has already been captured" << endl;
				}
			}
		}
	});

	// Extractor needs to get the root node. If no targets were added, add the root manually
	if (targets.size() == 0) {
		targets["_RootNode"].emplace(NodeAddress(program));
	}

	// Write out traversal results
	boost::filesystem::create_directory(args.outputFolder);
	string insertionLogfileName(args.outputFolder + "/" + "insertion_log.txt");
	fstream insertionLogfile(insertionLogfileName, fstream::out | fstream::trunc);
	if (!insertionLogfile.is_open()) {
		cerr << "Cannot open file " << insertionLogfileName << "!";
		return 1;
	}
	extractPointerFactsToFiles(targets, "", args.outputFolder, true, insertionLogfile);
	insertionLogfile.close();

	// dump IR code
	if(!args.dumpIR.empty()) {
		// Define file in which to print
		string irdumpFileName(args.outputFolder + "/" + args.dumpIR);
		fstream irdumpFile(irdumpFileName, fstream::out | fstream::trunc);
		if (!irdumpFile.is_open()) {
			cerr << "Cannot open file " << irdumpFileName << "!" << endl;
			return 1;
		}

		// Print in file
		cout << "Dumping intermediate representation to " << irdumpFileName << "..." << endl;
		irdumpFile << core::printer::PrettyPrinter(program, core::printer::PrettyPrinter::PRINT_DEREFS) << endl;

		irdumpFile.close();
	}

	// dump JSON
	if(!args.dumpJSON.empty()) {
		// Define file in which to print
		string jsondumpFileName(args.outputFolder + "/" + args.dumpJSON);
		fstream jsondumpFile(jsondumpFileName, fstream::out | fstream::trunc);
		if (!jsondumpFile.is_open()) {
			cerr << "Cannot open file " << jsondumpFileName << "!" << endl;
			return 1;
		}

		cout << "Dumping JSON to " << jsondumpFileName << "..." << endl;
		core::dump::json::dumpIR(jsondumpFile, program);

		jsondumpFile.close();
	}

	return 0;
}
