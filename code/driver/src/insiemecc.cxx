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

/**
 * Within this file a small, simple example of a compiler driver utilizing
 * the insieme compiler infrastructure is presented.
 *
 * This file is intended to provides a template for implementing new compiler
 * applications utilizing the Insieme compiler and runtime infrastructure.
 */

// Minimum size of the context string reported by the error checker
// (context will be extended when smaller)
#define MIN_CONTEXT 40

#define TEXT_WIDTH 120

#include <string>

#include <boost/algorithm/string.hpp>

#include "insieme/analysis/cfg.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/scopregion.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/extensions/ocl_host_extension.h"
#include "insieme/frontend/extensions/gemsclaim_extension.h"
#include "insieme/transform/polyhedral/scoppar.h"
#include "insieme/transform/polyhedral/scopvisitor.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/ocl_host/host_backend.h"
#include "insieme/backend/ocl_kernel/kernel_backend.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/object_file_utils.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_statistic.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/version.h"

using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace be = insieme::backend;
namespace dr = insieme::driver;
namespace cp = insieme::utils::compiler;
namespace cmd = insieme::driver::cmd;

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

//***************************************************************************************
// 				 STATS: show statistics about the IR
//***************************************************************************************
void showStatistics(const core::ProgramPtr& program) {
	openBoxTitle("IR Statistics");
	utils::measureTimeFor<INFO>("ir.statistics ", [&]() {
		LOG(INFO) << "\n" << core::IRStatistic::evaluate(program);
	});
	closeBox();
}

//****************************************************************************************
//                BENCHMARK CORE: Perform some performance benchmarks
//****************************************************************************************
void benchmarkCore(const core::NodePtr& program) {

	core::NodeManager& mgr = program->getNodeManager();

	openBoxTitle("Core Benchmarking");

	int count = 0;
	// Benchmark pointer-based visitor
	utils::measureTimeFor<INFO>("Benchmark.IterateAll.Pointer ",
		[&]() {
			core::visitDepthFirst(program,
				core::makeLambdaVisitor([&](const core::NodePtr& cur) { count++; }, true)
			);
		}
	);
	LOG(INFO) << "Number of nodes: " << count;

	// Benchmark address based visitor
	utils::Timer visitAddrTime("");
	count = 0;
	utils::measureTimeFor<INFO>("Benchmark.IterateAll.Address ",
		[&]() {
			core::visitDepthFirst(core::ProgramAddress(program),
				core::makeLambdaVisitor([&](const core::NodeAddress& cur) { count++; }, true)
			);
		}
	);
	LOG(INFO) << "Number of nodes: " << count;

	// Benchmark empty-substitution operation
	count = 0;
	utils::measureTimeFor<INFO>("Benchmark.IterateAll.Address ",
		[&]() {
			core::SimpleNodeMapping* h;
			auto mapper = core::makeLambdaMapper([&](unsigned, const core::NodePtr& cur)->core::NodePtr {
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
			core::SimpleNodeMapping* h2;
			auto mapper2 = core::makeLambdaMapper([&](unsigned, const core::NodePtr& cur)->core::NodePtr {
				if (cur->getNodeCategory() == core::NC_Type) {
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
	closeBox();
}

//****************************************************************************************
//            DUMP CFG: build the CFG of the program and dumps it into a file
//****************************************************************************************
void dumpCFG(const NodePtr& program, const std::string& outFile) {
	using namespace insieme::analysis;
	if(outFile.empty()) { return; }

	utils::Timer timer();
	CFGPtr graph = utils::measureTimeFor<CFGPtr, INFO>("Build.CFG", [&]() {
		return CFG::buildCFG<OneStmtPerBasicBlock>(program);
	});

	utils::measureTimeFor<INFO>( "Visit.CFG", [&]() {
			std::fstream dotFile(outFile.c_str(), std::fstream::out | std::fstream::trunc);
			dotFile << *graph;
		}
	);
}

//***************************************************************************************
// 					SEMA: Performs semantic checks on the IR
//***************************************************************************************
int checkSema(const core::NodePtr& program, core::checks::MessageList& list) {
	int retval = 0;

	using namespace insieme::core::printer;

	openBoxTitle("IR Semantic Checks");

	utils::measureTimeFor<INFO>("Semantic Checks ",
		[&]() { list = core::checks::check( program ); }
	);

	auto errors = list.getAll();
	std::sort(errors.begin(), errors.end());
	for_each(errors, [&](const core::checks::Message& cur) {
		LOG(INFO) << cur;
		core::NodeAddress address = cur.getOrigin();
		stringstream ss;
		unsigned contextSize = 1;
		do {

			ss.str("");
			ss.clear();
			core::NodePtr&& context = address.getParentNode(
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
															   PrettyPrinter::JUST_OUTERMOST_SCOPE |
															   PrettyPrinter::PRINT_CASTS) << std::endl;
		}

//		LOG(INFO) << "\t All: " << PrettyPrinter(address.getRootNode());
	});

	// In the case of semantic errors, quit
	if ( !list.getErrors().empty() ) {
		cerr << "---- Semantic errors encountered!! ----\n";
		retval = 1;
	}

	closeBox();
	return retval;
}

//***************************************************************************************
//		Polyhedral Model Extraction: Start analysis for SCoPs and prints out stats
//***************************************************************************************
void markSCoPs(core::ProgramPtr& program) {
	using namespace insieme::analysis::polyhedral::scop;

	// find SCoPs in our current program
	std::vector<core::NodeAddress> scoplist = utils::measureTimeFor<std::vector<core::NodeAddress>, INFO>("IR.SCoP.Analysis ",
		[&]() -> std::vector<core::NodeAddress> { return mark(program); });

	size_t numStmtsInScops = 0, loopNests = 0, maxLoopNest = 0;

	// loop over all SCoP annotations we have discovered
	std::for_each(scoplist.begin(), scoplist.end(),	[&](std::list<core::NodeAddress>::value_type& cur){
		ScopRegion& reg = *cur->getAnnotation(ScopRegion::KEY);

		// only print SCoPs which contain statement, at the user's request
		if(reg.getScop().size() == 0)
			return;

		std::cout << reg.getScop();

		// count number of statements covered by SCoPs
		numStmtsInScops += reg.getScop().size();

		// count maximum and average loop nesting level
		size_t loopNest = reg.getScop().nestingLevel();
		if(loopNest > maxLoopNest)
			maxLoopNest=loopNest;
		loopNests += loopNest;
	});

//	LOG(INFO) << std::setfill(' ') << std::endl
//			  << "SCoP Analysis" << std::endl
//			  << "\t# of SCoPs: " << sl.size() << std::endl
//			  << "\t# of stmts within SCoPs: " << numStmtsInScops << std::endl
//			  << "\tavg stmts per SCoP: " << std::setprecision(2) << (double)numStmtsInScops/sl.size() << std::endl
//			  << "\tavg loop nests per SCoP: " << std::setprecision(2) << (double)loopNests/sl.size() << std::endl
//			  << "\tmax loop nests per SCoP: " << maxLoopNest << std::endl;
}

//***************************************************************************************
//		Polyhedral Model Transformation: schedule relevant transformations
//***************************************************************************************
const core::ProgramPtr SCoPTransformation(const core::ProgramPtr& program, const cmd::Options& options) {
	int ocltransform = 0;

	// check whether OpenCL processing has been requested by the user
	auto oclChecker = [&]() -> bool {
	    for(auto extPtr : options.job.getExtensions()) {
            if(dynamic_cast<insieme::frontend::extensions::OclHostExtension*>(extPtr.get()))
                return true;
	    }
	    return false;
	};
	if (options.backendHint == cmd::BackendEnum::OpenCL || oclChecker()) {
		if ((options.backendHint == cmd::BackendEnum::OpenCL) ^ (oclChecker()))
			std::cerr << "Specify both the --opencl and --backend ocl options for OpenCL semantics!" << std::endl <<
						 "Not doing polyhedral OpenCL transformation." << std::endl;
		else {
			ocltransform=1;
			LOG(DEBUG) << "We will be using the backend: " << options.backendHint << "\n";
		}
	}

	// OCL transformations will - for now - happen on the old PM implementation; otherwise we choose the new one
	if (ocltransform) {
		std::vector<core::NodeAddress> scoplist=insieme::analysis::polyhedral::scop::mark(program);
		scoplist.clear(); // we do not use the scoplist right now, but we may want to refer to it later
		return insieme::transform::polyhedral::SCoPPar(program).apply();
	} else {
		auto pm=insieme::transform::polyhedral::novel::Polyhedral(ProgramAddress(program));
		// do some transformation here
		return pm.IR().getAddressedNode();
	}
}

//***************************************************************************************
//									Backend selection
//***************************************************************************************
insieme::backend::BackendPtr getBackend(const core::ProgramPtr& program, const cmd::Options& options) {

	if(options.backendHint == cmd::BackendEnum::OpenCL) {
		// use the OCL kernel backend if a KernelFctAnnotation can be found in the program,
		// otherwise use the OCL host backend
		bool host = [&]() {
			const auto& ep = program->getEntryPoints();
			for (auto& e : ep) {
				if(e->hasAnnotation(insieme::annotations::ocl::BaseAnnotation::KEY)) {
					auto annotations = e->getAnnotation(insieme::annotations::ocl::BaseAnnotation::KEY);
					for(const auto& ann : annotations->getAnnotationList())
						if(!dynamic_pointer_cast<insieme::annotations::ocl::KernelFctAnnotation>(ann))
							return true;
				} else
					return true;
			}
			return false;
		}();

		if(host)
			return be::ocl_host::OCLHostBackend::getDefault(options.settings.dumpOclKernel.string());
		else
			return be::ocl_kernel::OCLKernelBackend::getDefault(options.settings.dumpOclKernel.string());
	}

	if(options.backendHint == cmd::BackendEnum::Sequential)
		return be::sequential::SequentialBackend::getDefault();

    auto gemsclaimChecker = [&]() -> bool {
	    for(auto extPtr : options.job.getExtensions()) {
            if(dynamic_cast<insieme::frontend::extensions::GemsclaimExtension*>(extPtr.get()))
                return true;
	    }
	    return false;
	};
	return be::runtime::RuntimeBackend::getDefault(options.settings.estimateEffort, gemsclaimChecker());
}

int main(int argc, char** argv) {

	// Step 1: parse input parameters
    std::vector<std::string> arguments(argv, argv + argc);
    cmd::Options options = cmd::Options::parse(arguments);

 	Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr(options.settings.logLevel), options.settings.verbosity);

	// if options are invalid, exit non-zero
	if(!options.valid)
		return 1;

	// if e.g. help was specified, exit with zero
	if(options.gracefulExit)
		return 0;

	std::cout << "Insieme compiler - Version: " << utils::getVersion() << "\n";

	// Step 2: filter input files
	vector<fe::path> inputs;
	vector<fe::path> libs;
	vector<fe::path> extLibs;

	for(const fe::path& cur : options.job.getFiles()) {
		auto ext = fs::extension(cur);
		if (ext == ".o" || ext == ".so") {
			if (dr::isInsiemeLib(cur)) {
				libs.push_back(cur);
			} else {
				extLibs.push_back(cur);
			}
		} else {
			inputs.push_back(cur);
		}
	}
	// indicates that a shared object file should be created
	bool createSharedObject = fs::extension(options.settings.outFile) == ".so";

//std::cout << "Libs:    " << libs << "\n";
//std::cout << "Inputs:  " << inputs << "\n";
//std::cout << "ExtLibs: " << extLibs << "\n";
//std::cout << "OutFile: " << options.settings.outFile << "\n";
//std::cout << "Compile Only: " << options.settings.compileOnly << "\n";
//std::cout << "SharedObject: " << createSharedObject << "\n";
//std::cout << "WorkingDir: " << boost::filesystem::current_path() << "\n";

	// update input files
	options.job.setFiles(inputs);

    // Step 3: load input code
	co::NodeManager mgr;

	// load libraries
	options.job.setLibs(::transform(libs, [&](const fe::path& cur) {
		std::cout << "Loading " << cur << " ...\n";
		return dr::loadLib(mgr, cur);
	}));


	// if it is compile only or if it should become an object file => save it
	if(options.settings.compileOnly || createSharedObject) {
		auto res = options.job.toIRTranslationUnit(mgr);
		std::cout << "Saving object file ...\n";
		dr::saveLib(res, options.settings.outFile);
		return dr::isInsiemeLib(options.settings.outFile) ? 0 : 1;
	}

	// dump the translation unit file (if needed for de-bugging)
	if(!options.settings.dumpTU.empty()) {
		std::cout << "Dumping Translation Unit ...\n";
		auto tu = options.job.toIRTranslationUnit(mgr);
		std::ofstream out(options.settings.dumpTU.string());
		out << tu;
	}


	std::cout << "Extracting executable ...\n";

	// convert src file to target code
    auto program = options.job.execute(mgr);

    core::checks::MessageList errors;
    if(options.settings.checkSema || options.settings.checkSemaOnly) {
    	int retval = checkSema(program, errors);
    	if(options.settings.checkSemaOnly)
    		return retval;
    }

    if(!options.settings.dumpCFG.empty())
    	dumpCFG(program, options.settings.dumpCFG.string());

    if(options.settings.showStatistics)
    	showStatistics(program);

    if(options.settings.benchmarkCore)
    	benchmarkCore(program);

    if(options.settings.markScop)
    	markSCoPs(program);

    if(options.settings.usePM)
    	SCoPTransformation(program, options);

	// dump IR code
	if(!options.settings.dumpIR.empty()) {
		std::cout << "Dumping intermediate representation ...\n";
		std::ofstream out(options.settings.dumpIR.string());
		out << co::printer::PrettyPrinter(program, co::printer::PrettyPrinter::PRINT_DEREFS);
	}

	// Step 3: produce output code
	std::cout << "Creating target code ...\n";
	backend::BackendPtr backend = getBackend(program, options);
	auto targetCode = backend->convert(program);

	// dump source file if requested, exit if requested
	fe::path filePath = options.settings.dumpTRG;
	if(!options.settings.dumpTRGOnly.empty())
		filePath = options.settings.dumpTRGOnly;
	if(!filePath.empty()) {
		std::cout << "Dumping target code ...\n";
		std::ofstream out(filePath.string());
		out << *targetCode;
		if(!options.settings.dumpTRGOnly.empty())
			return 0;
	}


	// Step 4: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	//		if any of the translation units is has cpp belongs to cpp code, we'll use the
	//		cpp compiler, C otherwise
	cp::Compiler compiler =
			(options.job.isCxx())
				? cp::Compiler::getDefaultCppCompiler()
				: cp::Compiler::getDefaultC99Compiler();

	switch(options.backendHint.backend) {
		case cmd::BackendEnum::Sequential: break;
		case cmd::BackendEnum::Runtime:
		case cmd::BackendEnum::OpenCL:
		case cmd::BackendEnum::Pthreads:
		default: compiler = cp::Compiler::getRuntimeCompiler(compiler);
	}

    //add needed library flags
    for(auto cur : extLibs) {
		string libname = cur.filename().string();
		// add libraries by splitting their paths, truncating the filename of the library in the process (lib*.so*)
		compiler.addExternalLibrary(cur.parent_path().string(), libname.substr(3,libname.find(".")-3));
	}

	//add needed includeDirs for intercepted stuff
	for(auto cur : options.job.getInterceptedHeaderDirs()) {
		compiler.addIncludeDir(cur.string());
	}

    //add the given optimization flags (-f flags)
    for(auto cur : options.job.getFFlags()) {
        compiler.addFlag(cur);
    }

    //add definitions
    for(auto cur : options.job.getDefinitions()) {
        compiler.addFlag(std::string("-D" + cur.first));
    }

    //if an optimization flag is set (e.g. -O3)
    //set this flag in the backend compiler
    if(!options.settings.optimization.empty())
        compiler.addFlag("-O" + options.settings.optimization);
    if (options.settings.debug)
        compiler.addFlag("-g3");

    //check if the c++11 standard was set when calling insieme
    //if yes, use the same standard in the backend compiler
    if(options.job.isCxx()) {
        compiler.addFlag("-std=c++0x");
    }

	return !cp::compileToBinary(*targetCode, options.settings.outFile.string(), compiler);
}

