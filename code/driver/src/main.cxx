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
#include <memory>
#include <algorithm>
#include <fstream>

#include <boost/filesystem.hpp>

//movement of this line will lead to compile errors
//#include "insieme/frontend/translation_unit.h"

// Minimum size of the context string reported by the error checker
// (context will be extended when smaller)
#define MIN_CONTEXT 40

#include "insieme/core/ir_statistic.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/backend.h"
#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/ocl_kernel/kernel_backend.h"
#include "insieme/backend/ocl_host/host_backend.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/transform/ir_cleanup.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/filter/standard_filter.h"
#include "insieme/transform/polyhedral/transformations.h"
#include "insieme/transform/transformation.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/version.h"

#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/cilk/cilk_sema.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/utils/config.h"
#include "insieme/driver/printer/dot_printer.h"
#include "insieme/driver/pragma/pragma_transformer.h"
#include "insieme/driver/cmd/main_options.h"

#include "insieme/analysis/cfg.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/func_sema.h"

using namespace std;
using namespace insieme::utils::log;
using namespace insieme::annotations::ocl;
using namespace insieme::driver::cmd;

namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace be = insieme::backend;
namespace utils = insieme::utils;
namespace anal = insieme::analysis;

#define TEXT_WIDTH 80

namespace {

	void openBoxTitle(const std::string title) {
		LOG(INFO) <<
			// Opening ascii row
			"\n//" << std::setfill('*') << std::setw(TEXT_WIDTH) << std::right << "//" <<
			// Section title left aligned
			"\n//" << std::setfill(' ') << std::setw(TEXT_WIDTH-2) << std::left <<
				" " + title + " " << std::right << "//" <<
			// Closing ascii row
			"\n//" << std::setfill('*') << std::setw(TEXT_WIDTH) << "//";
	}

	void closeBox() {
		LOG(INFO) << "\n//" << std::setfill('=') << std::setw(TEXT_WIDTH) << "";
	}

	//****************************************************************************************
	//                BENCHMARK CORE: Perform some performance benchmarks
	//****************************************************************************************
	void doBenchmarkCore(NodeManager& mgr, const NodePtr& program, const CommandLineOptions& options) {

		if (!options.BenchmarkCore) return;

		int count = 0;
		// Benchmark pointer-based visitor
		utils::measureTimeFor<INFO>("Benchmark.IterateAll.Pointer ",
			[&]() {
				core::visitDepthFirst(program,
					core::makeLambdaVisitor([&](const NodePtr& cur) { count++; }, true)
				);
			}
		);
		LOG(INFO) << "Number of nodes: " << count;

		// Benchmark address based visitor
		utils::Timer visitAddrTime("");
		count = 0;
		utils::measureTimeFor<INFO>("Benchmark.IterateAll.Address  ",
			[&]() {
				core::visitDepthFirst(core::ProgramAddress(program),
					core::makeLambdaVisitor([&](const NodeAddress& cur) { count++; }, true)
				);
			}
		);
		LOG(INFO) << "Number of nodes: " << count;

		// Benchmark empty-substitution operation
		count = 0;
		utils::measureTimeFor<INFO>("Benchmark.IterateAll.Address  ",
			[&]() {
				NodeMapping* h;
				auto mapper = makeLambdaMapper([&](unsigned, const NodePtr& cur)->NodePtr {
					count++;
					return cur->substitute(mgr, *h);
				});
				h = &mapper;
				mapper.map(0,program);
			}
		);
		LOG(INFO) << "Number of modifications: " << count;

		// Benchmark empty-substitution operation (non-types only)
		count = 0;
		utils::measureTimeFor<INFO>("Benchmark.NodeSubstitution.Non-Types ",
			[&]() {
				NodeMapping* h2;
				auto mapper2 = makeLambdaMapper([&](unsigned, const NodePtr& cur)->NodePtr {
					if (cur->getNodeCategory() == NC_Type) {
						return cur;
					}
					count++;
					return cur->substitute(mgr, *h2);
				});
				h2 = &mapper2;
				mapper2.map(0,program);
			}
		);
		LOG(INFO) << "Number of modifications: " << count;
	}



	//****************************************************************************************
	//            DUMP CFG: build the CFG of the program and dumps it into a file
	//****************************************************************************************
	void dumpCFG(const NodePtr& program, const std::string& outFile) {
		if(outFile.empty()) { return; }

		utils::Timer timer();
		anal::CFGPtr graph = utils::measureTimeFor<anal::CFGPtr, INFO>("Build.CFG", [&]() {
			return anal::CFG::buildCFG<anal::OneStmtPerBasicBlock>(program);
		});

		utils::measureTimeFor<INFO>( "Visit.CFG", [&]() {
				std::fstream dotFile(outFile.c_str(), std::fstream::out | std::fstream::trunc);
				dotFile << *graph;
			}
		);
	}


	//***************************************************************************************
	// 				IR Pretty Print: Prints out the IR in textual form
	//***************************************************************************************
	void printIR(const NodePtr& program, const CommandLineOptions& options) {
		using namespace insieme::core::printer;

		if ( !options.PrettyPrint && options.DumpIR.empty() ) { return; }

		// A pretty print of the AST
		utils::measureTimeFor<DEBUG>("IR.PrettyPrint ", [&]() {
			openBoxTitle("Pretty Print INSPIRE");

			if(!options.DumpIR.empty()) {
				// write into the file
				std::fstream fout(options.DumpIR,  std::fstream::out | std::fstream::trunc);
				fout << "// -------------- Pretty Print Inspire --------------" << std::endl;
				fout << PrettyPrinter(program, PrettyPrinter::PRINT_DEREFS);
				fout << "====================================================================================================================================" << std::endl;
				fout << "====================================================================================================================================" << std::endl;
				fout << std::endl << std::endl;
				fout << "// --------- Pretty Print Inspire - Detail ----------" << std::endl;
				fout << PrettyPrinter(program, PrettyPrinter::OPTIONS_MAX_DETAIL);
				return;
			}
			std::cout << PrettyPrinter( program, PrettyPrinter::PRINT_DEREFS );
		});

		closeBox();
	}


	//***************************************************************************************
	// 					SEMA: Performs semantic checks on the IR
	//***************************************************************************************
	void checkSema(const core::NodePtr& program, MessageList& list, const CommandLineOptions& options) {

		using namespace insieme::core::printer;

		// Skip semantics checks if the flag is not set
		if (!options.CheckSema) { return; }

		openBoxTitle("IR Semantic Checks");

		utils::measureTimeFor<INFO>("Semantic Checks ",
			[&]() { list = check( program ); }
		);

		auto errors = list.getAll();
		std::sort(errors.begin(), errors.end());
		for_each(errors, [&](const Message& cur) {
			LOG(INFO) << cur;
			NodeAddress address = cur.getOrigin();
			stringstream ss;
			unsigned contextSize = 1;
			do {

				ss.str("");
				ss.clear();
				NodePtr&& context = address.getParentNode(
						min((unsigned)contextSize, address.getDepth()-contextSize)
					);
				ss << PrettyPrinter(context, PrettyPrinter::OPTIONS_SINGLE_LINE, 1+2*contextSize);

			} while(ss.str().length() < MIN_CONTEXT && contextSize++ < 5);
	//		LOG(INFO) << "\t Source-Node-Type: " << address->getNodeType();
			LOG(INFO) << "\t Source: " << PrettyPrinter(address, PrettyPrinter::OPTIONS_SINGLE_LINE);
			LOG(INFO) << "\t Context: " << ss.str() << std::endl;

			// find enclosing function
			auto fun = address;
			while(!fun.isRoot() && fun->getNodeType() != core::NT_LambdaExpr) {
				fun = fun.getParentAddress();
			}
			if (fun->getNodeType() == core::NT_LambdaExpr) {
				LOG(INFO) << "\t Context:\n" << PrettyPrinter(fun, PrettyPrinter::PRINT_DEREFS | 
																   PrettyPrinter::JUST_OUTHERMOST_SCOPE |
																   PrettyPrinter::PRINT_CASTS) << std::endl;
			}

	//		LOG(INFO) << "\t All: " << PrettyPrinter(address.getRootNode());
		});

		// In the case of semantic errors, quit
		if ( !list.getErrors().empty() ) {
			cerr << "---- Semantic errors encountered - compilation aborted!! ----\n";
			exit(1);
		}

		closeBox();
	}

	/// Polyhedral Model Extraction: Start analysis for SCoPs and prints out stats
	void markSCoPs(ProgramPtr& program, MessageList& errors, const CommandLineOptions& options) {
		if (!options.MarkScop && !options.UsePM) return;
		using namespace anal::polyhedral::scop;

		// find SCoPs in our current program
		AddressList sl = utils::measureTimeFor<AddressList, INFO>("IR.SCoP.Analysis ",
			[&]() -> AddressList { return mark(program); });

		size_t numStmtsInScops=0, loopNests=0, maxLoopNest=0;

		// loop over all SCoP annotations we have discovered
		std::for_each(sl.begin(), sl.end(),	[&](AddressList::value_type& cur){
			ScopRegion& reg = *cur->getAnnotation(ScopRegion::KEY);

			// only print SCoPs which contain statement, at the user's request
			if (reg.getScop().size()==0) return;
			else { if (options.MarkScop) { std::cout << reg.getScop(); } }

			// count number of statements covered by SCoPs
			numStmtsInScops += reg.getScop().size();

			// count maximum and average loop nesting level
			size_t loopNest = reg.getScop().nestingLevel();
			if (loopNest>maxLoopNest) maxLoopNest=loopNest;
			loopNests += loopNest;
		});

		LOG(INFO) << std::setfill(' ') << std::endl
				  << "SCoP Analysis" << std::endl
				  << "\t# of SCoPs: " << sl.size() << std::endl
				  << "\t# of stmts within SCoPs: " << numStmtsInScops << std::endl
				  << "\tavg stmts per SCoP: " << std::setprecision(2) << (double)numStmtsInScops/sl.size() << std::endl
				  << "\tavg loop nests per SCoP: " << std::setprecision(2) << (double)loopNests/sl.size() << std::endl
				  << "\tmax loop nests per SCoP: " << maxLoopNest << std::endl;
	}

	/// Polyhedral Model Transformation: check command line option and schedule relevant transformations
	ProgramPtr& SCoPTransformation(ProgramPtr& program, const CommandLineOptions& options) {
		if (!options.UsePM) return program;
		std::cout << "### We will be using the backend: " << options.Backend << std::endl;

		// filter SCoPs and build up the polyhedral transformation pipeline
		using namespace insieme::transform;
		std::vector<TransformationPtr> tr;
		tr.push_back(std::make_shared<insieme::transform::polyhedral::LoopParallelize>());
		auto transform=makePipeline(makeForAll(insieme::transform::filter::outermostSCoPs(), makePipeline(tr)));

		// apply transformation and return resulting program
		program = transform->apply(program);
		return program;
	}

	//***************************************************************************************
	// 				GRAPH DUMP: Dump the IR in a graphical form using DOT
	//***************************************************************************************
	void showIR(const core::ProgramPtr& program, MessageList& errors, const CommandLineOptions& options) {
		// Creates dot graph of the generated IR
		if(options.ShowIR.empty()) { return; }

		openBoxTitle("Dump IR Graph");
		utils::measureTimeFor<INFO>("Show.graph",
			[&]() {
				std::fstream dotOut(options.ShowIR.c_str(), std::fstream::out | std::fstream::trunc);
				insieme::driver::printer::printDotGraph(program, errors, dotOut);
			}
		);
		closeBox();
	}

	//***************************************************************************************
	// 				 STATS: show statistics about the IR
	//***************************************************************************************
	void showStatistics(const core::ProgramPtr& program, const CommandLineOptions& options) {
		if (!options.ShowStats) { return; }

		openBoxTitle("IR Statistics");
		utils::measureTimeFor<INFO>("ir.statistics ", [&]() {
			LOG(INFO) << "\n" << IRStatistic::evaluate(program);
		});
		closeBox();
	}


	std::pair<std::string, be::BackendPtr> selectBackend(const core::ProgramPtr& program, const CommandLineOptions& options) {

		// get option
		char selection = options.Backend[0];
		if (selection < 'a') { // to lower case
			selection += 'a' - 'A';
		}

		switch(selection) {
			case 'o': {
				// check if the host is in the entrypoints, otherwise use the kernel backend
				bool host = [&]() {
					const auto& ep = program->getEntryPoints();
					for (auto& e : ep) {
						if(e->hasAnnotation(BaseAnnotation::KEY)) {
							auto annotations = e->getAnnotation(BaseAnnotation::KEY);
							for(const auto& ann : annotations->getAnnotationList())
								if(!dynamic_pointer_cast<KernelFctAnnotation>(ann))
									return true;
						} else
							return true;
					}
					return false;
				}();

				// check if a path to dump the binary representation of the kernel is passed (form:
				// -b ocl:PATH)
				std::string kernelDumpPath;
				size_t idx = options.Backend.find(":");
				if(idx != std::string::npos) {
					kernelDumpPath = options.Backend.substr(idx+1, options.Backend.size());
				}

				if (host) {
					return {
						"OpenCL.Host.Backend",
						be::ocl_host::OCLHostBackend::getDefault(kernelDumpPath)
					};
				}

				return {
						"OpenCL.Kernel.Backend",
						be::ocl_kernel::OCLKernelBackend::getDefault(kernelDumpPath)
					};

			}

			case 's':
				return { "Sequential.Backend", be::sequential::SequentialBackend::getDefault() };

			case 'r':
			default:
				return { "Runtime.Backend", be::runtime::RuntimeBackend::getDefault(options.EstimateEffort, options.GemCrossCompile) };
		}
	}

} // end anonymous namespace


/**
 * Insieme compiler entry point
 */
int main(int argc, char** argv) {

	CommandLineOptions options = CommandLineOptions::parse(argc, argv);
	if (!options.valid) return 0;		// it was a help or about request

	Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr(options.LogLevel), options.Verbosity);
	LOG(INFO) << "Insieme compiler - Version: " << utils::getVersion();

	core::NodeManager manager;
	core::ProgramPtr program = core::Program::get(manager);

	// first of all, parse the command line options and do backend selection
	string backendName;
	be::BackendPtr backend;
	if (!options.Backend.empty()) {
		std::tie(backendName, backend) = selectBackend(program, options);
		options.Backend=backendName; // sanitize user input
	}

	// try to read the input sources and do some benchmarking
	try {
		if(!options.InputFiles.empty()) {

			// run frontend conversion
			program = utils::measureTimeFor<core::ProgramPtr,INFO>("Frontend.convert ",
					[&]() { return options.toConversionJob().execute(manager); }
				);

			// Load known function semantics from the function database
			anal::loadFunctionSemantics(program->getNodeManager());

			// Check for annotations on IR nodes relative to transformations which should be applied,
			// and applies them.
			program = utils::measureTimeFor<ProgramPtr,INFO>("Pragma.Transformer",
					[&]() { return insieme::driver::pragma::applyTransformations(program); } );

			printIR(program, options);

			// perform checks
			MessageList errors;
			checkSema(program, errors, options);

			// Performs some benchmarks
			doBenchmarkCore(manager, program, options);

			// Dump the Inter procedural Control Flow Graph associated to this program
			dumpCFG(program, options.CFG);

			// Perform SCoP region analysis
			markSCoPs(program, errors, options);
			program=SCoPTransformation(program, options);

			// IR statistics
			showStatistics(program, options);

			// Creates dot graph of the generated IR
			showIR(program, errors, options);

		}

		// write program output only if a backend has been selected
		if (!backendName.empty()) {
			openBoxTitle("Converting to TargetCode");

			utils::measureTimeFor<INFO>( backendName, [&](){
				// convert code
				be::TargetCodePtr targetCode = backend->convert(program);
				LOG(INFO) << "\n" << *targetCode; // do logger output before writing to a file

				// select output target
				if(!options.Output.empty()) {
					// write result to file ...
					std::fstream outFile(options.Output, std::fstream::out | std::fstream::trunc);
					outFile << *targetCode;
					outFile.close();

					// TODO: reinstate rewriter when fractions of programs are supported as entry points
					return;
				}
			});

			closeBox();
		}

	} catch (fe::ClangParsingError& e) {
		cerr << "Error while parsing input file: " << e.what() << endl;
		exit(1);
	}
}

