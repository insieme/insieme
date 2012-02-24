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

#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <limits>
#include <ctime>

#include <omp.h>

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/test/integration_tests.h"

#include "insieme/frontend/frontend.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/transform/transformation.h"
#include "insieme/transform/primitives.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/polyhedral/transform.h"
#include "insieme/transform/rulebased/transformations.h"
#include "insieme/transform/dump/text_dump.h"
#include "insieme/transform/filter/standard_filter.h"

#include "insieme/driver/region/pfor_selector.h"
#include "insieme/driver/optimizer/random_transformation_generator.h"

	/**
	 * This executable is realizing the control flow required for optimizing
	 * programs using the Insieme compiler infrastructure.
	 */

	using namespace std;
	using namespace insieme;
	using namespace insieme::core;
//	using namespace driver;
//	using namespace region;
	namespace bpo = boost::program_options;
	namespace bfs = boost::filesystem;

	using namespace insieme::transform;
//	using namespace insieme::transform::polyhedral;
//	using namespace insieme::transform::rulebased;


	/**
	 * A struct aggregating command line options.
	 */
	struct CmdOptions {
		bool valid;
		unsigned numResults;
		float applicationLimit;
		vector<string> benchmarks;
		string inputFile;
		string resultFile;
	};

	/**
	 * Parses command line options for this executable. The given options are
	 * parsed and the results are written
	 */
	CmdOptions parseCommandLine(int argc, char** argv);

	/**
	 * Extracts a list of regions from the benchmarks selected within the program options.
	 */
	vector<NodePtr> collectRegions(NodeManager& manager, const CmdOptions& options);

	vector<TransformationPtr> loadInitialTransformations(const CmdOptions& options);

	void saveTransformations(const vector<TransformationPtr>& list, const CmdOptions& options);

	unsigned countNumEffected(const vector<NodePtr>& regions, const TransformationPtr& transform);

	driver::optimizer::RandomTransformGenerator prepairGenerator();

	TransformationPtr getNext(driver::optimizer::RandomTransformGenerator& generator);

	/**
	 * The Insieme Optimizer entry point.
	 */
	int main(int argc, char** argv) {
		Logger::setLevel(ERROR);

		cout << " --- Insieme Transformation Scout, Version 0.0..01beta ---- \n";

		// Load command line options
		CmdOptions options = parseCommandLine(argc, argv);
		if (!options.valid) { return 1; }

		// prepair the global manager
		NodeManager manager;

		// Load kernels
		cout << "Loading regions from benchmarks ...\n";
		vector<NodePtr> regions = collectRegions(manager, options);
		cout << "Collected a total of " << regions.size() << " region(s).\n";


		// Load initial transformations
		vector<TransformationPtr> transformations;
		if (!options.inputFile.empty()) {
			transformations = loadInitialTransformations(options);
			cout << "Loaded a total of " << transformations.size() << " initial transformations.\n";
		}


		// Search transformations
		cout << "------------\n";

		// define number of regions to be touched
		unsigned limit = (unsigned)(regions.size() * options.applicationLimit);
		if (limit == 0) limit = 1;

		cout << "Start searching " << options.numResults << " new transformations effecting at least " << limit << " regions ...\n";

		#pragma omp parallel
		{
			// copy kernel list to local manager
			NodeManager localMgr;

			vector<NodePtr> localRegions;
			#pragma omp critical
			{
				for(auto it = regions.begin(); it!=regions.end(); it++) {
					localRegions.push_back(localMgr.get(*it));
				}
			}

			// set up transformation generator
			auto generator = prepairGenerator();

			// seed generator using different thread-specific values
			generator.seed(omp_get_thread_num() + time(NULL));

			#pragma omp for schedule(dynamic,1)
			for(unsigned i=0; i < options.numResults; i++) {

				// search one more transformation
				bool found = false;
				while(!found) {

					// obtain a transformation
					TransformationPtr cur = getNext(generator);

					bool isKnown = false;

					// skip transformation if already included
					#pragma omp critical
					isKnown = containsPtrToTarget(transformations, cur);

					if (!isKnown) {
						// count number of times transformation can be applied
						unsigned num = countNumEffected(localRegions, cur);
						if (limit <= num) {
							// add transformation to global container
							#pragma omp critical
							{

								if (!containsPtrToTarget(transformations, cur)) {
									transformations.push_back(cur);
									cout << "Found one effecting " << num << " region(s) - #" << transformations.size() << ":\n";
									dump::dumpTransformation(cout, cur);
									cout << "\n\n";
								}
							}
							found = true;
						}
					}
				}
			}
		}

//		unsigned goal = options.numResults + transformations.size();
//		while(transformations.size() < goal) {
//
//			// obtain a transformation
//			TransformationPtr cur = getNext(generator);
//
//			// count number of times transformation can be applied
//			unsigned num = countNumEffected(regions, cur);
//			if (limit <= num) {
//				transformations.push_back(cur);
//			}
//		}

		// Store results
		cout << "------------\n";
		cout << "Saving results ...\n";
		saveTransformations(transformations, options);

		cout << "Done\n";
	}


	CmdOptions parseCommandLine(int argc, char** argv) {
		CmdOptions fail;
		fail.valid = false;

		// -- parsing -------------------------------------------

		// define options
		bpo::options_description desc("Supported Parameters");
		desc.add_options()
				("help,h", "produce help message")
				("benchmarks", 			bpo::value<vector<string>>(), 					"the benchmarks to be loaded to obtain a list of kernels")
				("output,o", 			bpo::value<string>()->default_value("res.t"), 	"the file the obtained transformations should be stored to")
				("input,i", 			bpo::value<string>()->default_value(""), 		"a list of initial transformations to be extended")
				("numResults,n", 		bpo::value<unsigned>()->default_value(5), 		"the number of transformations to be obtained; 0 = no-limit")
				("applicationLimit,l", 	bpo::value<float>()->default_value(0.0), 		"the fraction of kernels a valid transformation should be applicable to")
		;

		// define positional options (all options not being named)
		bpo::positional_options_description pos;
		pos.add("benchmarks", -1);

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

		// get list of benchmarks
		if (map.count("benchmarks")) {
			res.benchmarks = map["benchmarks"].as<vector<string>>();
		} else {
			cout << "No benchmarks specified!\n";
			return fail;
		}

		res.resultFile = map["output"].as<string>();
		res.inputFile = map["input"].as<string>();
		res.numResults = map["numResults"].as<unsigned>();
		res.applicationLimit = map["applicationLimit"].as<float>();

		// fix boundaries of limit
		res.applicationLimit = (0.0 > res.applicationLimit)?0.0f:res.applicationLimit;
		res.applicationLimit = (res.applicationLimit > 1.0)?1.0f:res.applicationLimit;

		// accumulation complete
		return res;
	}

	vector<NodePtr> collectRegions(NodeManager& manager, const CmdOptions& options) {
		vector<NodePtr> res;
		for(unsigned i=0; i<options.benchmarks.size(); i++) {

			// start loading the full benchmark
			cout << "  Loading benchmark " << options.benchmarks[i] << " ... \n";

			// resolve test case setup
			const string& name = options.benchmarks[i];
			auto curCase = utils::test::getCase(name);
			if (!curCase) {
				cout << "  Unable to resolve test case " << name << "!\n";
				exit(1);
			}

			// load code using frontend
			auto job = frontend::ConversionJob(manager, curCase->getFiles(), curCase->getIncludeDirs());
			job.setOption(frontend::ConversionJob::OpenMP);
			ProgramPtr program = job.execute();

			// extract kernels
			cout << "  Extracting regions ... \n";

			// collect all pfor-bodies
			vector<driver::region::Region> regions = driver::region::PForBodySelector().getRegions(program);
			cout << "  Found " << regions.size() << " region(s).\n";
			for_each(regions, [&](const driver::region::Region& cur) {
				res.push_back(cur);
			});
		}
		return res;
	}

	vector<TransformationPtr> loadInitialTransformations(const CmdOptions& options) {

		// check whether there are any initial transformations
		if (options.inputFile.empty()) {
			return vector<TransformationPtr>();
		}

		// load transformations from input file
		fstream in(options.inputFile, fstream::in);
		vector<TransformationPtr> res = dump::loadTransformations(in, getStandardCatalog());
		in.close();
		return res;
	}

	bool checkTransferDump(const vector<TransformationPtr>& list) {
		// save transformation within stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a text format
		dump::dumpTransformations(buffer, list);

		// restore the transformation
		const Catalog& catalog = getStandardCatalog();
		vector<TransformationPtr> restored = dump::loadTransformations(buffer, catalog);

		// check whether restoring the list was successful
		return equals(list, restored, equal_target<TransformationPtr>());
	}

	void saveTransformations(const vector<TransformationPtr>& list, const CmdOptions& options) {

		// make sure list can be restored before touching current list
		if (!checkTransferDump(list)) {
			std::cerr << "WARNING: prevented un-restorable list to be stored!";
			return;
		}

		// write result to file ...
		fstream outFile(options.resultFile, fstream::out | fstream::trunc);
		dump::dumpTransformations(outFile, list);
		outFile.close();
	}

	unsigned countNumEffected(const vector<NodePtr>& regions, const TransformationPtr& transform) {
		unsigned counter = 0;

		core::NodeManager manager;
		for_each(regions, [&](const NodePtr& cur) {
			try {
				if (*cur != *transform->apply(manager.get(cur))) {
					counter++;
				}
			} catch (const InvalidTargetException& ite) {}
		});

		return counter;
	}

	driver::optimizer::RandomTransformGenerator prepairGenerator() {
		vector<TransformationTypePtr> types = toVector<TransformationTypePtr>(

				// basic loop transformations
				&polyhedral::LoopInterchangeType::getInstance(),
				&polyhedral::LoopStripMiningType::getInstance(),
				&polyhedral::LoopTilingType::getInstance(),
				&polyhedral::LoopFusionType::getInstance(),
				&polyhedral::LoopFissionType::getInstance(),
				&rulebased::LoopUnrollingType::getInstance()

		);
		vector<filter::Filter> filters = toVector(filter::all);
		vector<filter::TargetFilter> targetFilters = toVector(
				filter::outermostSCoPs(),
				filter::outermostLoops(),
				filter::innermostLoops(),
				filter::innermostLoops(2),
				filter::innermostLoops(2),
				filter::innermostLoops(3)
		);
		driver::optimizer::RandomTransformGenerator generator(types, filters, targetFilters);

		// register some specific handlers
		generator.registerGenerator(&polyhedral::LoopTilingType::getInstance(),
				[](driver::optimizer::RandomTransformGenerator& gen)->TransformationPtr {
					int numLoops = gen.pickRandom(2,3); // tile between 2 and 3 loops
					vector<unsigned> sizes;
					for(int i=0; i<numLoops; i++) {
						sizes.push_back(gen.pickRandom(2, 128));
					}
					return polyhedral::makeLoopTiling(sizes);
		});

		generator.registerGenerator(&rulebased::LoopUnrollingType::getInstance(),
				[](driver::optimizer::RandomTransformGenerator& gen)->TransformationPtr {
					return rulebased::makeLoopUnrolling(gen.pickRandom(2,128));
		});

		return generator;
	}

	TransformationPtr getNext(driver::optimizer::RandomTransformGenerator& gen) {
		// to simplify the implementation, the format is fixed
		// it will always be a
		//		for-all ( X , pipeline ( Ys ... ) )
		// transformation

		// pick X
		filter::TargetFilter filter = gen.pickRandom(gen.getTargetFilterPool());

		// pick a number for the elements within the pipeline
		unsigned size = gen.pickRandom(1,5);
		vector<TransformationPtr> list;
		for(unsigned i =0; i<size; i++) {
			list.push_back(gen.next());
		}
		return makeForAll(filter, makePipeline(list));
	}
