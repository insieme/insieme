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
#include "insieme/utils/timer.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/analysis/modeling/cache.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/handle_fetures.h"

	/**
	 * This executable is accepting some program code and extracting a set of
	 * feature from the given program.
	 */

	using namespace std;
	using namespace insieme;
	namespace bpo = boost::program_options;
	namespace bfs = boost::filesystem;
	namespace ft = insieme::analysis::features;

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
		core::NodeAddress code = driver::loadCode(manager, options.inputs, options.includes, options.definitions);

		// print code fragment:
		cerr << "Processing Code Fragment: \n" << core::printer::PrettyPrinter(code) << "\n\n";

		// obtain list of features
		vector<ft::FeaturePtr> features = driver::getFeatureList();

		// extract all features
		vector<ft::Value> values = driver::extractFeatures(code, features);

		// extract features
		for(std::size_t i = 0; i<features.size(); i++) {
			cerr << format("%-60s %20s\n", features[i]->getName().c_str(), toString(values[i]).c_str());
		}

		// write features into a file
		// TODO: implement

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



	bool hasArrayOrVectorSubType(const core::TypePtr& type) {
		auto checker = core::makeLambdaVisitor([](const core::TypePtr& type)->bool {
			return type->getNodeType() == core::NT_VectorType || type->getNodeType() == core::NT_ArrayType;
		}, true);
		auto arrayOrVectorSearcher = core::makeDepthFirstOnceInterruptibleVisitor(checker);

		return arrayOrVectorSearcher.visit(type);
	}

	uint64_t countVectorArrayCreations(const core::NodePtr& node) {
		const auto& basic = node->getNodeManager().getLangBasic();
		int64_t res = 0;
		core::visitDepthFirstOnce(node,
				[&](const core::CallExprPtr& call) {
					auto fun = call->getFunctionExpr();
					if (!basic.isRefVar(fun) && !basic.isRefNew(fun)) {
						return;
					}
					core::TypePtr type = core::analysis::getReferencedType(call->getType());
					if (hasArrayOrVectorSubType(type)) {
						std::cerr << "Found one: " << *call << "\n";
						res++;
					}
		});
		return res;
	}


	void processDirectory(const CmdOptions& options) {

		// access root directory
		bfs::path dir(options.kernelRootDir);
		std::cerr << "Processing directory: " << dir << "\n";

		if (!bfs::is_directory(dir)) {
			std::cerr << "Not a directory!" << std::endl;
			return;
		}

		// collect features
		vector<ft::FeaturePtr> features = driver::getFeatureList();

		vector<bfs::path> kernels;
		for (auto it = bfs::recursive_directory_iterator(dir);
				it != bfs::recursive_directory_iterator(); ++it) {

			auto kernelFile = it->path() / "kernel.dat";


			if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) > 500000) {
				std::cerr << "Ignoring Large File: " << kernelFile << "\n";
				continue;
			}

			if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) < 500000) {
				kernels.push_back(kernelFile);
			}
		}

		std::cerr << "Found " << kernels.size() << " kernels!" << std::endl;



		// print head line
		std::cout << "Benchmark;Kernel;Version;" << join(";",features, print<deref<ft::FeaturePtr>>()) << "\n";



		// process all identifies kernels ...
		#pragma omp parallel
		{
			core::NodeManager manager;

			#pragma omp for schedule(dynamic,1)
			for (std::size_t i=0; i<kernels.size(); i++) {
				auto path = kernels[i];

				try {

					std::cout << "Processing Kernel " << path.string() << "\n";

					fstream in(path.string(), fstream::in);
					auto kernelCode = core::dump::binary::loadAddress(in, manager);

					auto version 	= path.parent_path();
					auto kernel 	= version.parent_path();
					auto benchmark 	= kernel.parent_path();

					utils::Timer timer("Simulation Time");
					vector<ft::Value> values = driver::extractFeatures(kernelCode, features);
					timer.stop();

//					uint64_t num_allocs = countVectorArrayCreations(kernelCode);

//					utils::Timer timer("Reusedistance ..");
//					size_t reuseDistance = analysis::modeling::getReuseDistance(kernelCode);
//					timer.stop();

					#pragma omp critical
					{
						std::cout << benchmark.filename()
								<< "; " << kernel.filename()
								<< "; " << version.filename()
								<< "; " << ::join(";", values)
//								<< "; " << num_allocs
//								<< "; " << reuseDistance
								<< "; " << (long)(timer.getTime()*1000)
								<< "\n";
					}

				} catch (const core::dump::InvalidEncodingException& iee) {
					std::cerr << "Invalid encoding within kernel file of " << path;
				}
			}
		}

	}

