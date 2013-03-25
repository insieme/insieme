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
#include "insieme/transform/polyhedral/transformations.h"
#include "insieme/transform/rulebased/transformations.h"
#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/filter/standard_filter.h"

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
		bool papi;
		bool oneKernelVersionPerFile;
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
			: pfor(core::static_address_cast<core::CallExprAddress>(body.getParentAddress(7))), body(body) {
			assert(core::analysis::isCallOf(pfor.getAddressedNode(), body->getNodeManager().getLangBasic().getPFor()) && "No pfor at expected position!");
		}

		Kernel(const core::StatementAddress& pfor)
			: pfor(core::static_address_cast<core::CallExprAddress>(pfor)),
			  body(core::static_address_cast<core::CompoundStmtAddress>(pfor.getAddressOfChild(5,2,1,2,0,1,2))) {}

		Kernel() {}

		operator bool() const { return pfor; }
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

	core::CompoundStmtPtr instrumentKernel(const core::StatementPtr& kernel, int id);

	vector<transform::TransformationPtr> getTransformationPool();

	void createVersionFiles(const CmdOptions& options, const transform::TransformationPtr& transform, const core::ProgramPtr& program, const Kernel& kernels, bfs::path dir);
	void createVersionFiles(const CmdOptions& options, const transform::TransformationPtr& transform, const core::ProgramPtr& program, const vector<Kernel>& kernels, bfs::path dir);

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
			if (kernels.empty()) {
				cout << "No kernels found within file!\n";
				return 1;
			}
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
			if (options.oneKernelVersionPerFile) {

				// create an individual kernel / variant / kernel.c file for each kernel/variant combination

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

							Kernel kernel = kernels[i];
							bfs::path kernel_dir = dir / format("kernel_%d", i);

							try {

								const transform::TransformationPtr& transform = pool[j];

								// apply transformation on region
								core::StatementPtr transformed = transform->apply(tmp.get(kernel.body.as<core::StatementPtr>()));

								// check whether versions has already been covered
								bool contains = true;

								#pragma omp critical (versionComparison)
								{
									contains = versions[i].contains(transformed);

									// register version
									versions[i].insert(transformed);
								}

								if (!contains) {

									// create modified program - start by replacing body
									kernel = Kernel(core::transform::replaceAddress(tmp, kernel.body, transformed).as<core::CompoundStmtAddress>());
									core::ProgramPtr version = kernel.body.getRootNode().as<core::ProgramPtr>();

									// create files
									cout << "Creating files for kernel #" << i << " version #" << j << " ... \n";
									createVersionFiles(options, transform, version, kernel, kernel_dir / format("version_%d", j));

								}

							} catch (const transform::InvalidTargetException& ite) {
								// ignored => try next version ...
								cout << "Transfromation #" << j << " could not be applyied for kernel #" << i << " ... \n";

							}
						}
					}
				}
			} else {

				// put all kernel variants into a single file
				#pragma omp parallel
				{
					core::NodeManager localManager;

					#pragma omp critical
					localManager.setNextFreshID(manager.getFreshID());

					#pragma omp for schedule(dynamic,1)
					for (unsigned j=0; j<pool.size(); j++) {

						#pragma omp critical
						cout << "Transforming kernels for version #" << j << " ...\n";

						const transform::TransformationPtr& transform = pool[j];

						// compute transformed regions
						int successCounter = 0;
						vector<core::StatementPtr> transformed;
						for(unsigned i=0; i<kernels.size(); i++) {
							try {

								core::StatementPtr res = transform->apply(localManager.get(kernels[i].body.as<core::StatementPtr>()));

								// check whether code was without any effect
								if (*transform == *makeNoOp() || *res != *kernels[i].body) {
									successCounter++;
									transformed.push_back(res);
									continue;
								}

							} catch(const transform::InvalidTargetException& ite) {
							}
							transformed.push_back(core::StatementPtr());
						}

						#pragma omp critical
						cout << "Creating file for version #" << j << " including " << successCounter << " modified kernel(s) ... \n";

						// check whether at least one transformation was successful
						if (successCounter == 0) {
							// skip this variant
							continue;
						}

						// create modified program
						core::NodePtr version = kernels[0].body.getRootNode();
						vector<Kernel> modifiedKernels = kernels;
						for(unsigned i=0; i<modifiedKernels.size(); i++) {
							if (transformed[i]) {
								// create modified program - start by replacing body
								modifiedKernels[i] = Kernel(core::transform::replaceAddress(localManager, modifiedKernels[i].body.switchRoot(version), transformed[i]).as<core::CompoundStmtAddress>());
								version = modifiedKernels[i].body.getRootNode();
							} else {
								modifiedKernels[i] = Kernel();
							}
						}

						// fix root nodes of all modified kernels
						for_each(modifiedKernels, [&](Kernel& cur) {
							if (cur) {
								cur.body = cur.body.switchRoot(version);
								cur.pfor = cur.pfor.switchRoot(version);
							}
						});

						// create files
						bfs::path version_dir = dir / format("version_%d", j);
						createVersionFiles(options, transform, version.as<core::ProgramPtr>(), modifiedKernels, version_dir);

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
				("one-kernel-variant-per-file,s", "every kernel variant will be written into an independent file")
				("link-papi,p","link papi library")
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

		// add flag determining whether each kernel version should be written into an extra file
		res.oneKernelVersionPerFile = map.count("one-kernel-variant-per-file");

		// link papi library
		res.papi = map.count("link-papi");

		// create result
		return res;
	}


	core::ProgramPtr loadSources(core::NodeManager& manager, const CmdOptions& options) {
		// use frontend to load program files
		auto job = frontend::ConversionJob(options.inputs, options.includes);
		job.setOption(frontend::ConversionJob::OpenMP);
		job.setDefinitions(options.definitions);
		return job.execute(manager);
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
		auto outermost			= filter::outermostLoops();
		auto innermost 			= filter::innermostLoops();
		auto secondInnermost  	= filter::innermostLoops(2);
		auto thirdInnermost 	= filter::innermostLoops(3);
		auto outermostSCoPs 	= filter::outermostSCoPs();

		// use original code ...
		res.push_back(transform::makeNoOp());


		// -- tiling --

		// tile 2 innermost loops
		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling({8,8}))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling({16,16}))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling({4,4}))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling({8,4}))
		));

		res.push_back(makeForAll(
			secondInnermost,
			makeTry(makeLoopTiling({4,8}))
		));

		res.push_back(makeForAll(
			thirdInnermost,
			makeTry(makeLoopTiling({4,4,4}))
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
					makeLoopTiling({32,64})
			)),
			makeTry(makeForAll(
					secondInnermost,
					makeLoopTiling({4,16})
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
					makeLoopTiling({4,16})
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

	bool checkBinary(const bfs::path& path, const vector<core::NodeAddress>& kernels) {
		assert(!kernels.empty() && "Cannot check for empty kernel list!");

		core::NodeManager& manager = kernels[0]->getNodeManager();
		fstream in(path.string(), fstream::in);
		vector<core::NodeAddress> restored = core::dump::binary::loadAddresses(in, manager);
		in.close();
		auto range = make_paired_range(kernels, restored);
		return all(range.first, range.second, [](const std::pair<core::NodeAddress, core::NodeAddress>& cur) {
			return *cur.first.getRootNode() == *cur.second.getRootNode() && cur.first == cur.second;
		});
	}

	core::CompoundStmtPtr instrumentKernel(const core::StatementPtr& kernel, int id) {

		auto& manager = kernel->getNodeManager();
		auto& basic = manager.getLangBasic();
		auto& ext = manager.getLangExtension<insieme::backend::runtime::Extensions>();
		core::IRBuilder builder(manager);

		auto startCall = builder.callExpr(basic.getUnit(), ext.instrumentationRegionStart, builder.intLit(id));
		auto endCall = builder.callExpr(basic.getUnit(), ext.instrumentationRegionEnd, builder.intLit(id));

		return builder.compoundStmt(startCall, kernel, endCall);
	}

	void createVersionFiles(const CmdOptions& options, const transform::TransformationPtr& transform, const core::ProgramPtr& program, const Kernel& kernel, bfs::path dir) {
		createVersionFiles(options, transform, program, toVector(kernel), dir);
	}

	void createVersionFiles(const CmdOptions& options, const transform::TransformationPtr& transform, const core::ProgramPtr& program, const vector<Kernel>& kernels, bfs::path dir) {

		// check pre-condition
		assert(all(kernels, [&](const Kernel& cur) { return (!cur) || (program == cur.body.getRootNode() && program == cur.pfor.getRootNode()); }));

		// assemble directory location

		// check whether file already exists
		if (bfs::exists(dir)) {
			std::cerr << "WARNING: directory " << dir << " already exists - skipping file creation!\n";
			return;
		}

		// create directory
		bfs::create_directories(dir);

		// add instrumentation to all kernels
		core::NodeManager& manager = program->getNodeManager();
		core::ProgramPtr instrumented = program;
		unsigned id = 0;
		for_each(kernels, [&](const Kernel& cur) {
			if (cur) {
				core::StatementAddress pfor = cur.pfor.switchRoot(instrumented);
				instrumented = core::transform::replaceNode(manager, pfor, instrumentKernel(pfor, id)).as<core::ProgramPtr>();
			}
			id++;
		});

		// create kernel code
		auto backend = insieme::backend::runtime::RuntimeBackend::getDefault();
		backend->setOperatorTableExtender(&insieme::driver::isolator::addOpSupport);
		toFile(dir / "kernel.c", *backend->convert(instrumented));

		// compile c file
		if (options.build) {
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
			compiler.addFlag("-I " SRC_DIR "../../runtime/include -g -O0 -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -ldl -lrt -lpthread -lm");
			if(options.papi)
				compiler.addFlag("-I " PAPI_HOME "/include -L " PAPI_HOME "/lib -lpapi");
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
		vector<core::NodeAddress> kernelAddresses;
		for_each(kernels, [&](const Kernel& kernel) {
			if (kernel) kernelAddresses.push_back(kernel.body);
		});
		toFile(dir / "kernel.dat", core::dump::binary::BinaryDump(kernelAddresses));

		// make sure, binary version is correct
		if (!checkBinary(dir / "kernel.dat", kernelAddresses)) {
			std::cerr << "Error creating binary dump of files!\n";
		}

		#pragma omp critical (XML_DUMP)
		{
		#ifdef USE_XML
			// save data within xml too - just to be safe
			xml::XmlUtil::write(program, (dir / "kernel.xml").string());
		#endif
		}

		// add transformation info file
		toFile(dir / "transform.info", *transform);

		// add kernel code using the pretty printer
		std::stringstream txt;
		id = 0;
		for_each(kernels, [&](const Kernel& kernel) {
			if (kernel) {
				txt << "// Kernel " << id << ": \n";
				txt << core::printer::PrettyPrinter(kernel.body, core::printer::PrettyPrinter::OPTIONS_DETAIL) << "\n\n";
			}
			id++;
		});
		toFile(dir / "kernel.ir", txt.str());
	}
