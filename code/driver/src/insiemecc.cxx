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

#include <string>

#include <boost/algorithm/string.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/frontend/frontend.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/driver/cmd/insiemecc_options.h"
#include "insieme/driver/object_file_utils.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/timer.h"


using namespace std;
using namespace insieme;

namespace fs = boost::filesystem;

namespace fe = insieme::frontend;
namespace co = insieme::core;
namespace be = insieme::backend;
namespace dr = insieme::driver;
namespace cp = insieme::utils::compiler;
namespace cmd = insieme::driver::cmd;

//****************************************************************************************
//                BENCHMARK CORE: Perform some performance benchmarks
//****************************************************************************************
void benchmarkCore(core::NodeManager& mgr, const core::NodePtr& program) {
	Logger::setLevel(INFO);

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
			core::NodeMapping* h;
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
			core::NodeMapping* h2;
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
}
/*
/// Polyhedral Model Extraction: Start analysis for SCoPs and prints out stats
	void markSCoPs(core::ProgramPtr& program, const cmd::Options& options) {
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

//		LOG(INFO) << std::setfill(' ') << std::endl
//				  << "SCoP Analysis" << std::endl
//				  << "\t# of SCoPs: " << sl.size() << std::endl
//				  << "\t# of stmts within SCoPs: " << numStmtsInScops << std::endl
//				  << "\tavg stmts per SCoP: " << std::setprecision(2) << (double)numStmtsInScops/sl.size() << std::endl
//				  << "\tavg loop nests per SCoP: " << std::setprecision(2) << (double)loopNests/sl.size() << std::endl
//				  << "\tmax loop nests per SCoP: " << maxLoopNest << std::endl;
	}

	/// Polyhedral Model Transformation: check command line option and schedule relevant transformations
	const core::ProgramPtr SCoPTransformation(const core::ProgramPtr& program, const cmd::Options& options) {
		int ocltransform=0;

		// check whether OpenCL processing has been requested by the user
		if (options.Backend=="OpenCL.Host.Backend" || options.OpenCL) {
			if ((options.Backend=="OpenCL.Host.Backend") ^ (options.OpenCL))
				std::cerr << "Specify both the --opencl and --backend ocl options for OpenCL semantics!" << std::endl <<
							 "Not doing polyhedral OpenCL transformation." << std::endl;
			else {
				ocltransform=1;
				LOG(DEBUG) << "We will be using the backend: " << options.Backend << std::endl;
			}
		}

		// OCL transformations will - for now - happen on the old PM implementation; otherwise we choose the new one
		if (ocltransform) {
			std::vector<core::NodeAddress> scoplist=insieme::analysis::polyhedral::scop::mark(program);
			scoplist.clear(); // we do not use the scoplist right now, but we may want to refer to it later
			return insieme::transform::polyhedral::SCoPPar(program).apply();
		} else {
			auto scoplist=insieme::transform::polyhedral::novel::SCoPVisitor(ProgramAddress(program)).scoplist;
			// do some transformation here
			return scoplist.IR().getAddressedNode();
		}
	}
*/
int main(int argc, char** argv) {
	// filter logging messages
	Logger::setLevel(ERROR);

	// Step 1: parse input parameters
	//		This part is application specific and needs to be customized. Within this
	//		example a few standard options are considered.

 	cmd::Options options = cmd::Options::parse(argc, argv);

	//indicates that a shared object file should be created
	bool createSharedObject = options.settings.outFile.string().find(".so") != std::string::npos;

	// if options are invalid, exit non-zero
	if(!options.valid)
		return 1;

	// if help was specified, exit with zero
	if(options.gracefulExit)
		return 0;

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

//std::cout << "Libs:    " << libs << "\n";
//std::cout << "Inputs:  " << inputs << "\n";
//std::cout << "ExtLibs: " << extLibs << "\n";
//std::cout << "OutFile: " << options.outFile << "\n";
//std::cout << "Compile Only: " << compileOnly << "\n";
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
		dr::saveLib(res, options.settings.outFile);
		return dr::isInsiemeLib(options.settings.outFile) ? 0 : 1;
	}

	// dump the translation unit file (if needed for de-bugging)
	if(!options.settings.dumpTU.empty()) {
		std::cout << "Converting Translation Unit ...\n";
		auto tu = options.job.toIRTranslationUnit(mgr);
		std::ofstream out(options.settings.dumpTU.string());
		out << tu;
	}

	std::cout << "Extracting executable ...\n";

	// convert src file to target code
    auto program = options.job.execute(mgr);

    if(options.settings.benchmarkCore) {
    	std::cout << "benchmarking" << "\n";
    	benchmarkCore(mgr, program);
    }

	// dump IR code
	if(!options.settings.dumpIR.empty()) {
		std::ofstream out(options.settings.dumpIR.string());
		out << co::printer::PrettyPrinter(program, co::printer::PrettyPrinter::PRINT_DEREFS);
	}

	if(options.job.hasOption(fe::ConversionSetup::StrictSemanticChecks)) {
		std::cout << "Running semantic checks ...\n";
		auto list = co::checks::check(program);
		if (!list.empty()){
			auto errors = list.getAll();
			std::sort(errors.begin(), errors.end());
			for_each(errors, [&](const co::checks::Message& cur) {
				std::cout << cur << std::endl;
				co::NodeAddress address = cur.getOrigin();
				stringstream ss;
				unsigned contextSize = 1;
				do {

					ss.str("");
					ss.clear();
					co::NodePtr&& context = address.getParentNode(
							min((unsigned)contextSize, address.getDepth()-contextSize)
						);
					ss << co::printer::PrettyPrinter(context, co::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 1+2*contextSize);

				} while(ss.str().length() < 40 && contextSize++ < 5);

				// find enclosing function
				auto fun = address;
				while(!fun.isRoot() && fun->getNodeType() != co::NT_LambdaExpr) {
					fun = fun.getParentAddress();
				}
				if (fun->getNodeType() == co::NT_LambdaExpr) {
					std::cout << "\tContext:\n" << co::printer::PrettyPrinter(fun,co::printer::PrettyPrinter::PRINT_DEREFS |
													co::printer::PrettyPrinter::PRINT_CASTS |
													co::printer::PrettyPrinter::JUST_OUTHERMOST_SCOPE) << std::endl; 
					if(!co::annotations::hasNameAttached(fun)) {
						std::cout << " Function originaly named: " << co::annotations::getAttachedName(fun)<< std::endl;
					}
				}
			});
		}
	}

	// Step 3: produce output code
	//		This part converts the processed code into C-99 target code using the
	//		backend producing parallel code to be executed using the Insieme runtime
	//		system. Backends targeting alternative platforms may be present in the
	//		backend modul as well.
	cout << "Creating target code ...\n";
	auto targetCode = be::runtime::RuntimeBackend::getDefault(options.settings.estimateEffort, options.job.hasOption(fe::ConversionJob::GemCrossCompile))->convert(program);

	// dump source file if requested
	if(!options.settings.dumpTRG.empty()) {
		std::ofstream out(options.settings.dumpTRG.string());
		out << *targetCode;
	}


	// Step 4: build output code
	//		A final, optional step is using a third-party C compiler to build an actual
	//		executable.
	//		if any of the translation units is has cpp belongs to cpp code, we'll use the
	//		cpp compiler, C otherwise
	cout << "Building binaries ...\n";

	cp::Compiler compiler =
			(options.job.isCxx())
				? cp::Compiler::getDefaultCppCompiler()
				: cp::Compiler::getDefaultC99Compiler();

	compiler = cp::Compiler::getRuntimeCompiler(compiler);

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

    //FIXME: Add support for -O1, -O2, ... 
    //if an optimization flag is set (e.g. -O3) 
    //set this flag in the backend compiler
    if(options.settings.fullOptimization)
        compiler.addFlag("-O3");
    if (options.settings.debug)
        compiler.addFlag("-g3");

    //check if the c++11 standard was set when calling insieme
    //if yes, use the same standard in the backend compiler
    if(options.job.getStandard() == fe::ConversionSetup::Standard::Cxx11) {
        compiler.addFlag("-std=c++0x");
    }
    
	bool success = cp::compileToBinary(*targetCode, options.settings.outFile.string(), compiler);

	// done
	return (success)?0:1;
}
