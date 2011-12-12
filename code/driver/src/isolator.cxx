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

#include <iostream>
#include <string>
#include <vector>
#include <fstream>

#include <boost/program_options.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/checks/ir_checks.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/omp/omp_sema.h"

#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/transform/catalog.h"

#include "insieme/driver/optimizer/random_optimizer.h"
#include "insieme/driver/predictor/dummy_predictor.h"
#include "insieme/driver/region/size_based_selector.h"
#include "insieme/driver/region/pfor_selector.h"

	/**
	 * This executable is realizing the control flow required for optimizing
	 * programs using the Insieme compiler infrastructure.
	 */

	using namespace std;
	using namespace insieme;
	using namespace driver;
	using namespace transform;
	using namespace region;
	namespace bpo = boost::program_options;

	/**
	 * A struct aggregating command line options.
	 */
	struct CmdOptions {
		bool valid;
		vector<string> inputs;
		vector<string> includes;
	};


	/**
	 * Parses command line options for this executable. The given options are
	 * parsed and the results are written
	 */
	CmdOptions parseCommandLine(int argc, char** argv);

	/**
	 * Loads the program input files and converts it into the IR.
	 */
	core::ProgramPtr loadSources(core::NodeManager& manager, const CmdOptions& options);

	vector<core::NodeAddress> preprocessRegions(const vector<Region>& regions);

	void profileProgram(const core::ProgramPtr& program, const vector<core::NodeAddress>& regions);

	/**
	 * The Insieme Optimizer entry point.
	 */
	int main(int argc, char** argv) {
		core::NodeManager manager;

		try {

			// set up logger
			Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("ERROR"));

			cout << " --- Insieme Kernel Isolator, Version 0.0..01beta ---- \n";

			// Step 1) handle command line arguments
			CmdOptions options = parseCommandLine(argc, argv);
			if (!options.valid) {
				return 1;
			}

			// Step 2) load sources
			cout << "Loading sources ... \n\t" << join("\n\t", options.inputs) << "\n";
			core::ProgramPtr program = loadSources(manager, options);


			// Step 3) identify regions
			cout << "Selecting regions ... " << std::flush;
			auto regions = preprocessRegions(PForBodySelector().getRegions(program));
			cout << regions.size() << " regions selected.\n";


			// Step 4) profile program
			profileProgram(program, regions);

			// Step 5) create isolated kernel codes

			// done
			cout << "Done!\n";
			return 0;

		} catch (frontend::ClangParsingError& e) {
			cerr << "Unexpected error encountered: " << e.what() << endl;
			exit(1);
		}
	}


	CmdOptions parseCommandLine(int argc, char** argv) {
		CmdOptions fail;
		fail.valid = false;

		// -- parsing -------------------------------------------

		// define options
		bpo::options_description desc("Supported Parameters");
		desc.add_options()
				("help,h", "produce help message")
				("input-file,i", bpo::value<vector<string>>(), "input files - required!")
				("include-path,I", bpo::value<vector<string>>(), "include files")
				("output-file,o", bpo::value<string>(), "output file - default: out.c")
		;

		// define positional options (all options not being named)
		bpo::positional_options_description pos;
		pos.add("input-file", -1);

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

		CmdOptions res;
		res.valid = true;

		// input files
		if (map.count("input-file")) {
			res.inputs = map["input-file"].as<vector<string>>();
		} else {
			cout << "No input files provided!\n";
			return fail;
		}


		// include files (optional)
		if (map.count("include-path")) {
			res.includes = map["include-path"].as<vector<string>>();
		}

//		// output file
//		if (map.count("output-file")) {
//			res.resultFile = map["output-file"].as<string>();
//		} else {
//			res.resultFile = "out.c";
//		}


		// create result
		return res;
	}


	core::ProgramPtr loadSources(core::NodeManager& manager, const CmdOptions& options) {
		// use frontend to load program files
		auto job = frontend::ConversionJob(manager, options.inputs, options.includes);
		job.setOption(frontend::ConversionJob::OpenMP);
		auto program = job.execute();
		program = frontend::omp::applySema(program, program->getNodeManager());
		return program;
	}

	vector<core::NodeAddress> preprocessRegions(const vector<Region>& regions) {

		// move up to pfor node
		vector<core::NodeAddress> res;
		for_each(regions, [&](const Region& cur) {
			core::NodeAddress pfor = cur.getParentAddress(8); // pfor is 8!! levels higher
			assert(core::analysis::isCallOf(pfor, cur->getNodeManager().getLangBasic().getPFor())
					&& "No pfor at expected position!");
			res.push_back(pfor); // pfor is 7 levels higher
		});

		return res;
	}

	void profileProgram(const core::ProgramPtr& program, const vector<core::NodeAddress>& regions) {
		std::cout << "Profiling Program ...\n";

		// process regions individually
		for_each(regions, [](const core::NodeAddress& cur) {

			// just log region
			cout << "\nProcessing Region: \n";
			cout << core::printer::PrettyPrinter(cur.getAddressedNode());
			cout << "\n";

			// compute free variables
			core::VariableList free = core::analysis::getFreeVariables(cur.getAddressedNode());

			// classify free variables
			for_each(free, [](const core::VariablePtr& var) {
				cout << "Free Variable: " << *var->getType() << " " << *var << "\n";
			});

			// add profiling code

		});
	}
