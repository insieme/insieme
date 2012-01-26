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

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/dump/binary_dump.h"

#include "insieme/analysis/features/code_feature_catalog.h"

#include "insieme/frontend/frontend.h"


	/**
	 * This executable is accepting some program code and extracting a set of
	 * feature from the given program.
	 */

	using namespace std;
	using namespace insieme;
	namespace bpo = boost::program_options;
	namespace bfs = boost::filesystem;

	/**
	 * A struct aggregating command line options.
	 */
	struct CmdOptions {
		bool valid;						/* < a flag determining whether the cmd options are valid or not. */
		string outputFile;				/* < the file to be used for storing the list of extracted features. */
		vector<string> inputs;			/* < the input file to be processed in case a source file is passed as an argument. */
		vector<string> includes;		/* < the includes to be considered when processing the input files. */
		vector<string> definitions;		/* < some definitions to be considered. */
		string kernelRootDir;			/* < the root directory of the extracted kernel files */
	};

	/**
	 * Parses command line options for this executable. The given options are
	 * parsed and the results are written
	 */
	CmdOptions parseCommandLine(int argc, char** argv);

	/**
	 *
	 */
	core::NodeAddress loadCode(core::NodeManager& manager, const CmdOptions& options);

	void processDirectory(const CmdOptions& options);

	/**
	 * The Insieme Optimizer entry point.
	 */
	int main(int argc, char** argv) {
		core::NodeManager manager;

		// set up logger
		Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("ERROR"));
		//Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("DEBUG"));

		cerr << " --- Insieme Code Feature Extractor, Version 0.0..01beta ---- \n";

		// process handle command line arguments
		CmdOptions options = parseCommandLine(argc, argv);
		if (!options.valid) {
			return 1;
		}

		if (!options.kernelRootDir.empty()) {
			processDirectory(options);
			return 0;
		}

		// loading features
		analysis::features::FeatureCatalog catalog = analysis::features::getFullCodeFeatureCatalog();
		cerr << "Supporting " << catalog.size() << " features.\n";

		// load code fragment
		cerr << "Loading input files ..." << endl;
		core::NodeAddress code = loadCode(manager, options);


		// print code fragment:
		cerr << "Processing Code Fragment: \n" << core::printer::PrettyPrinter(code) << "\n\n";


		// extract features
		for_each(catalog, [&](const std::pair<string, analysis::features::FeaturePtr>& cur) {
			cerr << format("%-60s %20.0f\n", cur.first.c_str(), analysis::features::getValue<double>(cur.second->extractFrom(code.getAddressedNode())));
			//cerr << "Feature: " << cur.first << "\t\tValue: " << cur.second->extractFrom(code.getAddressedNode()) << endl;
		});


		// write features into a file


		// done
		cerr << "Done!" << endl;
		return 0;

	}


	CmdOptions parseCommandLine(int argc, char** argv) {
		CmdOptions fail;
		fail.valid = false;

		// -- parsing -------------------------------------------

		// define options
		bpo::options_description desc("Supported Parameters");
		desc.add_options()
				("help,h", "produce help message")
				("directory,d", bpo::value<string>(), "root directory for kernel files to be processed")
				("input-file,i", bpo::value<vector<string>>(), "input files - required!")
				("include-path,I", bpo::value<vector<string>>(), "include files - optional")
				("definitions,D", bpo::value<vector<string>>(), "preprocessor definitions - optional")
				("output-file,o", bpo::value<string>(), "the file the list of features should be written to, default: console")
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
		}

		// output file (optional)
		res.outputFile = "";
		if (map.count("output-file")) {
			res.outputFile = map["output-file"].as<string>();
		}

		res.kernelRootDir = "";
		if (map.count("directory")) {
			res.kernelRootDir = map["directory"].as<string>();
		}

		// include path
		if (map.count("include-path")) {
			res.includes = map["include-path"].as<vector<string>>();
		}

		// preprocessor directives
		if (map.count("definitions")) {
			res.definitions = map["definitions"].as<vector<string>>();
		}

		if (res.inputs.empty() && res.kernelRootDir.empty()) {
			cout << "No input files provided!\n";
			return fail;
		}

		// create result
		return res;
	}


	core::NodeAddress loadCode(core::NodeManager& manager, const CmdOptions& options) {

		try {
			// check whether the given file is a binary file ..
			if (options.inputs.size() == 1u && !(boost::ends_with(options.inputs[0], ".c") || boost::ends_with(options.inputs[0], ".cpp"))) {
				// try loading the given binary file
				fstream in(options.inputs[0], fstream::in);
				return core::dump::binary::loadAddress(in, manager);
			}
		} catch (const core::dump::InvalidEncodingException& iee) {
			cerr << "Unable to decode binary input file: " << iee.what() << "\nTrying to load file using C/C++ frontend ..." << endl;
		}

		try {

			// use frontend to load program files
			auto job = frontend::ConversionJob(manager, options.inputs, options.includes);
			job.setOption(frontend::ConversionJob::OpenMP);
			job.setDefinitions(options.definitions);
			return core::NodeAddress(job.execute());

		} catch (const frontend::ClangParsingError& e) {
			cerr << "Unexpected error encountered: " << e.what() << endl;
			exit(1);
		}

		return core::NodeAddress();
	}


	void processDirectory(const CmdOptions& options) {

		core::NodeManager manager;

		// access root directory
		bfs::path dir(options.kernelRootDir);
		std::cout << "Processing directory: " << dir << "\n";

		if (!bfs::is_directory(dir)) {
			std::cout << "Not a directory!" << std::endl;
			return;
		}

		analysis::features::FeaturePtr feature = analysis::features::createSimpleCodeFeature("NumLoops", "",
				analysis::features::createNumForLoopSpec(analysis::features::FeatureAggregationMode::FA_Static));

		for (auto it = bfs::directory_iterator(dir); it != bfs::directory_iterator(); ++it) {
			std::cout << it->path().filename() << "\n";

			if (bfs::is_directory(it->path())) {
				auto benchmark = it->path().filename();

				for(auto it2 = bfs::directory_iterator(it->path()); it2!=bfs::directory_iterator(); ++it2) {

					if (bfs::is_directory(it2->path()) && it2->path().filename().substr(0, sizeof("kernel")-1) == "kernel") {
						auto kernel = it2->path().filename();
						for(auto it3 = bfs::directory_iterator(it2->path()); it3 != bfs::directory_iterator(); ++it3) {
							if (bfs::is_directory(it3->path()) && it3->path().filename().substr(0, sizeof("version")-1) == "version") {
								auto version = it3->path().filename();


								// load kernel

								try {
									auto kernelFile = it3->path() / "kernel.dat";

									if (!bfs::exists(kernelFile)) {
										std::cerr << "Unable to load kernel file for " << benchmark << "/" << kernel << "/" << version;
										continue;
									}

									fstream in(kernelFile.string(), fstream::in);
									auto kernelCode = core::dump::binary::loadAddress(in, manager);

									int value = (int)analysis::features::getValue<analysis::features::simple_feature_value_type>(feature->extractFrom(kernelCode.getAddressedNode()));

									std::cout << benchmark << "; " << kernel << "; " << version << "; " << value << ";\n";

								} catch (const core::dump::InvalidEncodingException& iee) {
									std::cerr << "Invalid encoding within kernel file of " << benchmark << "/" << kernel << "/" << version;
								}
							}
						}



					}

				}

			}

		}

	}
