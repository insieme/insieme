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
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/checks/ir_checks.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/dump/binary_dump.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/omp/omp_sema.h"

#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/transform/transformation.h"
#include "insieme/transform/catalog.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/polyhedral/transform.h"

#include "insieme/transform/rulebased/transformations.h"

#include "insieme/transform/pattern/ir_pattern.h"

#include "insieme/driver/region/size_based_selector.h"
#include "insieme/driver/region/pfor_selector.h"
#include "insieme/driver/isolator/isolator.h"
#include "insieme/driver/driver_config.h"

#ifdef USE_XML
#include "insieme/xml/xml_utils.h"
#endif



	/**
	 * This executable is realizing the control flow required for optimizing
	 * programs using the Insieme compiler infrastructure.
	 */

	using namespace std;
	using namespace insieme;
	using namespace driver;
	using namespace region;
	namespace bpo = boost::program_options;
	namespace bfs = boost::filesystem;

	using namespace insieme::transform;
	using namespace insieme::transform::polyhedral;
	using namespace insieme::transform::rulebased;


	/**
	 * A struct aggregating command line options.
	 */
	struct CmdOptions {
		bool valid;
		bool isolate;
		bool build;
		string benchmarkName;
		string outputDirectory;
		vector<string> inputs;
		vector<string> includes;
		vector<string> definitions;
	};

	/**
	 * A struct used to represent kernels.
	 */
	struct Kernel {
		core::CallExprAddress pfor;
		core::CompoundStmtAddress body;

		Kernel(const core::CompoundStmtAddress& body)
			: pfor(core::static_address_cast<core::CallExprAddress>(body.getParentAddress(8))), body(body) {
			assert(core::analysis::isCallOf(pfor.getAddressedNode(), body->getNodeManager().getLangBasic().getPFor()) && "No pfor at expected position!");
		}

		Kernel(const core::StatementAddress& pfor)
			: pfor(core::static_address_cast<core::CallExprAddress>(pfor)),
			  body(core::static_address_cast<core::CompoundStmtAddress>(pfor.getAddressOfChild(2,4,2,1,2,0,1,2))) {}

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

	vector<Kernel> extractKernels(const core::ProgramPtr& program);

	vector<Kernel> isolateKernels(const vector<Kernel>& regions, const string& contextFile);

	vector<transform::TransformationPtr> getTransformationPool();

	void createVersionFiles(const CmdOptions& options, const transform::TransformationPtr& transform,
			const Kernel& kernel, bfs::path dir);

	/**
	 * A utility to write printable content to a file.
	 */
	template<typename T>
	void toFile(const bfs::path& path, const T& content) {
		fstream out(path.string(), fstream::out | fstream::trunc);
		out << content;
		out.close();
	}

	/**
	 * The Insieme Optimizer entry point.
	 */
	int main(int argc, char** argv) {
		core::NodeManager manager;

		try {

			// set up logger
			Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("ERROR"));
			//Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("DEBUG"));

			cout << " --- Insieme Kernel Isolator, Version 0.0..01beta ---- \n";

			// Step 1) handle command line arguments
			CmdOptions options = parseCommandLine(argc, argv);
			if (!options.valid) {
				return 1;
			}

			// assemble directory location
			bfs::path dir = bfs::path(options.outputDirectory) / options.benchmarkName;

			// check whether file already exists
			if (bfs::exists(dir)) {
				std::cerr << "WARNING: directory " << dir << " already exists - skipping file creation!\n";
				return 1;
			}
			bfs::create_directories(dir);

			// Step 2) load sources
			cout << "Loading sources ... \n  " << join("\n  ", options.inputs) << "\n";
			core::ProgramPtr program = loadSources(manager, options);


			// Step 3) creating a program backup
			toFile(dir / "benchmark.dat", core::dump::binary::BinaryDump(program));
			#ifdef USE_XML
				// save data within xml too - just to be safe
				xml::XmlUtil::write(program, (dir / "benchmark.xml").string());
			#endif


			// Step 4) identify regions
			cout << "Selecting kernels ... " << std::flush;
			auto kernels = extractKernels(program);
			cout << kernels.size() << " kernel(s) selected.\n";

			// Step 5) load transformation pool
			vector<transform::TransformationPtr> pool = getTransformationPool();
			cout << "Loaded " << pool.size() << " Transformation(s)\n";

			// Step 6) isolate the kernels
			if (options.isolate) {
				cout << "Collecting context and isolating kernels ...";
				auto contextFile = dir / "context.dat";
				kernels = isolateKernels(kernels, contextFile.string());
			}

			vector<utils::set::PointerSet<core::NodePtr>> versions(kernels.size());


			// Step 7) create isolated kernel codes
			for(unsigned i=0; i < kernels.size(); i++) {
				const Kernel& kernel = kernels[i];

				// create directory
				bfs::path kernel_dir = dir / format("kernel_%d", i);
				bfs::create_directories(kernel_dir);

				// add kernel code using the pretty printer
				toFile(kernel_dir / "unmodified.ir", core::printer::PrettyPrinter(kernel.body.getAddressedNode(), core::printer::PrettyPrinter::OPTIONS_DETAIL));

			}
			// for each variant
			#pragma omp parallel
			{
				core::NodeManager tmp;

				#pragma omp for schedule(dynamic,1) collapse(2)
				for(unsigned i=0; i < kernels.size(); i++) {
					for(unsigned j=0; j<pool.size(); j++) {

						const Kernel& kernel = kernels[i];
						bfs::path kernel_dir = dir / format("kernel_%d", i);

						try {

							const transform::TransformationPtr& transform = pool[j];

							// apply transformation on region
							core::NodePtr transformed = transform->apply( 
									core::static_pointer_cast<const core::Node>(tmp.get(kernel.body.getAddressedNode())) );

							// check whether versions has already been covered
							bool contains = true;

							#pragma omp critical (versionComparison)
							{
								contains = versions[i].contains(transformed);

								// register version
								versions[i].insert(transformed);
							}

							if (!contains) {

								// create modified program
								core::NodePtr version = core::transform::replaceNode(tmp, kernel.body, transformed);

								// create files
								cout << "Creating files for kernel #" << i << " version #" << j << " ... \n";
								createVersionFiles(options, transform, Kernel(kernel.body.switchRoot(version)), kernel_dir / format("version_%d", j));

							}

						} catch (const transform::InvalidTargetException& ite) {
							// ignored => try next version ...
							cout << "Transfromation #" << j << " could not be applyied for kernel #" << i << " ... \n";

						}
					}
				}
			}

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
				("include-path,I", bpo::value<vector<string>>(), "include files - optional")
				("definitions,D", bpo::value<vector<string>>(), "preprocessor definitions - optional")
				("output-directory,d", bpo::value<string>(), "the output directory - default: .")
				("benchmark-name,n", bpo::value<string>(), "the name of the processed benchmark - default: benchmark")
				("capture-context,c", "enables the isolation of kernels by capturing the local context.")
				("build-kernel,b", "will also compile the extracted kernel.")
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

		// set isolation flag
		res.isolate = map.count("capture-context");

		// set build flag
		res.build = map.count("build-kernel");

		// input files
		if (map.count("input-file")) {
			res.inputs = map["input-file"].as<vector<string>>();
		} else {
			cout << "No input files provided!\n";
			return fail;
		}


		// output directory (optional)
		res.outputDirectory = ".";
		if (map.count("output-directory")) {
			res.outputDirectory = map["output-directory"].as<string>();
		}

		// benchmark name
		res.benchmarkName = "benchmark";
		if (map.count("benchmark-name")) {
			res.benchmarkName = map["benchmark-name"].as<string>();
		}
		// include path
		if (map.count("include-path")) {
			res.includes = map["include-path"].as<vector<string>>();
		}
		// preprocessor directives
		if (map.count("definitions")) {
			res.definitions = map["definitions"].as<vector<string>>();
		}
		// create result
		return res;
	}


	core::ProgramPtr loadSources(core::NodeManager& manager, const CmdOptions& options) {
		// use frontend to load program files
		auto job = frontend::ConversionJob(manager, options.inputs, options.includes);
		job.setOption(frontend::ConversionJob::OpenMP);
		job.setDefinitions(options.definitions);
		return job.execute();
//		auto program = job.execute();
//		program = frontend::omp::applySema(program, program->getNodeManager());
//		return program;
	}

	vector<Kernel> extractKernels(const core::ProgramPtr& program) {

		// collect all pfor-bodies
		vector<Region> regions = PForBodySelector().getRegions(program);

		// convert regions into kernels
		vector<Kernel> res;
		for_each(regions, [&](const Region& cur) {
			res.push_back(Kernel(cur));
		});

		return res;
	}

	vector<Kernel> isolateKernels(const vector<Kernel>& kernels, const string& contextFile) {
		assert(!kernels.empty() && "Does not work without regions!");
		assert(kernels[0].pfor.getRootNode()->getNodeType() == insieme::core::NT_Program);

		// extract program
		insieme::core::ProgramPtr program = static_pointer_cast<insieme::core::ProgramPtr>(kernels[0].pfor.getRootNode());

		// create list of regions
		vector<insieme::core::StatementAddress> regions;
		for_each(kernels, [&](const Kernel& cur) {
			regions.push_back(cur.pfor);
		});

		// instrument program for profiling run
		regions = driver::isolator::isolate(program, regions, contextFile);

		// convert regions back into kernels
		vector<Kernel> res;
		for_each(regions, [&](const insieme::core::StatementAddress& cur) {
			res.push_back(Kernel(cur));
		});

		return res;
	}

	vector<transform::TransformationPtr> getTransformationPool() {
		// TODO: load from file!

		vector<transform::TransformationPtr> res;

		// create some common filters
		auto outermost			= filter::pattern(transform::pattern::outermost(transform::pattern::var("x",transform::pattern::irp::forStmt())), "x");
		auto innermost 			= filter::allMatches(transform::pattern::irp::innerMostForLoopNest());
		auto secondInnermost  	= filter::allMatches(transform::pattern::irp::innerMostForLoopNest(2));
		auto thirdInnermost 	= filter::allMatches(transform::pattern::irp::innerMostForLoopNest(3));
		auto outermostSCoPs 	= filter::outermostSCoPs();

		// use original code ...
		res.push_back(transform::makeNoOp());


		// -- tiling --

		// tile 2 innermost loops
		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling(8,8))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling(16,16))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling(4,4))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling(8,4))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling(4,8))
		));

		res.push_back(makeForAll(
			thirdInnermost,
			makeTry(makeLoopTiling(4,4,4))
		));


		// -- unrolling --

		res.push_back(makeForAll(
			innermost,
			makeTry(makeLoopUnrolling(2))
		));

		res.push_back(makeForAll(
			innermost,
			makeTry(makeLoopUnrolling(8))
		));

		res.push_back(makeForAll(
			innermost,
			makeTry(makeLoopUnrolling(32))
		));

		res.push_back(makeForAll(
			outermost,
			makeTry(makeLoopUnrolling(4))
		));


		// -- interchange --

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopInterchange(0,1))
		));

		res.push_back(makeForAll(
			outermost,
			makeTry(makeLoopInterchange(0,1))
		));


		// -- some combinations --

		// distributed stuff as much as possible ...
		res.push_back(makeForAll(
				outermost,
				makeFixpoint(makeTry(makeLoopFission(1)), 1000)
		));


		// merge innermost loops
		res.push_back(makePipeline(
			makeTry(makeForAll(
					secondInnermost,
					makeLoopUnrolling(4)
			)),
			makeForAll(
					innermost,
					makeTry(makeLoopFusion(0,1,2,3))
			))
		);


		// tile for L3 and L2 ..
		res.push_back(makePipeline(
			makeTry(makeForAll(
					secondInnermost,
					makeLoopTiling(32,64)
			)),
			makeTry(makeForAll(
					secondInnermost,
					makeLoopTiling(4,16)
			)),
			makeTry(makeForAll(
					innermost,
					makeLoopUnrolling(4)
			))
		));

		// tile for L2 ...
		res.push_back(makePipeline(
			makeTry(makeForAll(
					secondInnermost,
					makeLoopTiling(4,16)
			)),
			makeTry(makeForAll(
					innermost,
					makeLoopUnrolling(4)
			))
		));

		// -- rescheduler --

		//res.push_back(makeForAll(
				//outermostSCoPs,
				//makeLoopReschedule()
		//));

		//res.push_back(makeForAll(
				//outermostSCoPs,
				//makePipeline(
						//makeLoopReschedule(),
						//makeForAll(
								//innermost,
								//makeLoopUnrolling(4)
						//)
				//)
		//));

		//res.push_back(makeForAll(
				//outermostSCoPs,
				//makePipeline(
						//makeLoopReschedule(),
						//makeForAll(
								//secondInnermost,
								//makeLoopTiling(4,16)
						//),
						//makeForAll(
								//innermost,
								//makeLoopUnrolling(4)
						//)
				//)
		//));

		return res;
	}

	bool checkBinary(const bfs::path& path, const core::NodeAddress& kernel) {
		core::NodeManager& manager = kernel->getNodeManager();
		fstream in(path.string(), fstream::in);
		core::NodeAddress restored = core::dump::binary::loadAddress(in, manager);
		in.close();
		return *restored.getRootNode() == *kernel.getRootNode() && restored == kernel;
	}

	core::ProgramPtr instrumentKernel(const core::StatementAddress& kernel) {
		assert(kernel.getRootNode()->getNodeType() == core::NT_Program);

		auto& manager = kernel->getNodeManager();
		auto& basic = manager.getLangBasic();
		auto& ext = manager.getLangExtension<insieme::backend::runtime::Extensions>();
		core::IRBuilder builder(manager);

		auto startCall = builder.callExpr(basic.getUnit(), ext.instrumentationRegionStart, builder.intLit(0));
		auto endCall = builder.callExpr(basic.getUnit(), ext.instrumentationRegionEnd, builder.intLit(0));

		core::StatementPtr encapsulated = builder.compoundStmt(startCall, kernel.getAddressedNode(), endCall);
		return static_pointer_cast<core::ProgramPtr>(core::transform::replaceNode(manager, kernel, encapsulated));
	}

	void createVersionFiles(const CmdOptions& options, const transform::TransformationPtr& transform, const Kernel& kernel, bfs::path dir) {
		assert(kernel.pfor.getRootNode()->getNodeType() == core::NT_Program);

		// assemble directory location

		// check whether file already exists
		if (bfs::exists(dir)) {
			std::cerr << "WARNING: directory " << dir << " already exists - skipping file creation!\n";
			return;
		}

		// create directory
		bfs::create_directories(dir);

		// create kernel code
		auto backend = insieme::backend::runtime::RuntimeBackend::getDefault();
		backend->setOperatorTableExtender(&insieme::driver::isolator::addOpSupport);
		toFile(dir / "kernel.c", *backend->convert(instrumentKernel(kernel.pfor)));

		// compile c file
		if (options.build) {
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
			compiler.addFlag("-I " SRC_DIR "../../runtime/include -g -O0 -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -ldl -lrt -lpthread -lm");
			compiler.addFlag("-DRESTORE");

			string src = (dir / "kernel.c").string();
			string trg = (dir / "binary").string();
			bool success = utils::compiler::compile(src, trg, compiler);
			if (!success) {
				LOG(ERROR) << "Unable to compile extracted kernel code.";
			}
			assert(success && "Unable to compile extracted kernel code.");
		}


		// safe binary dump
		toFile(dir / "kernel.dat", core::dump::binary::BinaryDump(core::NodeAddress(kernel.body)));

		// make sure, binary version is correct
		assert(checkBinary(dir / "kernel.dat", kernel.body));

		#pragma omp critical (XML_DUMP)
		{
		#ifdef USE_XML
			// save data within xml too - just to be safe
			xml::XmlUtil::write(kernel.pfor.getRootNode(), (dir / "kernel.xml").string());
		#endif
		}

		// add transformation info file
		toFile(dir / "transform.info", *transform);

		// add kernel code using the pretty printer
		toFile(dir / "kernel.ir", core::printer::PrettyPrinter(kernel.body.getAddressedNode(), core::printer::PrettyPrinter::OPTIONS_DETAIL));
	}
