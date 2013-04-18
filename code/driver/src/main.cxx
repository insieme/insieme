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

// Minimum size of the context string reported by the error checker
// (context will be extended when smaller)
#define MIN_CONTEXT 40

#include "insieme/core/ir_statistic.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/backend/backend.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/ocl_kernel/kernel_backend.h"
#include "insieme/backend/ocl_host/host_backend.h"

#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/transform.h"

#include "insieme/transform/ir_cleanup.h"
#include "insieme/transform/connectors.h"
#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/polyhedral/transformations.h"
#include "insieme/transform/rulebased/transformations.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/version.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/cilk/cilk_sema.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/driver/driver_config.h"
#include "insieme/driver/printer/dot_printer.h"
#include "insieme/analysis/region/size_based_selector.h"
#include "insieme/driver/pragma/pragma_transformer.h"
#include "insieme/driver/pragma/pragma_info.h"
#include "insieme/driver/cmd/main_options.h"

#ifdef USE_XML
#include "insieme/xml/xml_utils.h"
#endif

#include "insieme/analysis/cfg.h"

#include "insieme/analysis/dfa/problem.h"
#include "insieme/analysis/dfa/solver.h"
#include "insieme/analysis/dfa/analyses/const_prop.h"
#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/defuse_collect.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/analysis/mpi/comm_graph.h"
#include "insieme/analysis/dep_graph.h"
#include "insieme/analysis/func_sema.h"
#include "insieme/analysis/modeling/cache.h"

using namespace std;
using namespace insieme::utils::log;
using namespace insieme::annotations::ocl;
using namespace insieme::driver::cmd;

namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace be = insieme::backend;
#ifdef USE_XML
	namespace xml = insieme::xml;
#endif
namespace utils = insieme::utils;
namespace anal = insieme::analysis;

bool checkForHashCollisions(const ProgramPtr& program);

namespace {

template <class Ret=void>
Ret measureTimeFor(const std::string& timerName, const std::function<Ret ()>& task) {
	utils::Timer timer(timerName);
	Ret&& ret = task(); // execute the job
	timer.stop();
	LOG(INFO) << timer;
	return ret;
}

// Specialization for void returning functions 
template <>
void measureTimeFor<void>(const std::string& timerName, const std::function<void ()>& task) {
	utils::Timer timer(timerName);
	task(); // execute the job
	timer.stop();
	LOG(INFO) << timer;
}

//****************************************************************************************
//                BENCHMARK CORE: Perform some performance benchmarks
//****************************************************************************************
void doBenchmarkCore(NodeManager& mgr, const NodePtr& program, const CommandLineOptions& options) {

	if (!options.BenchmarkCore) return;

	int count = 0;
	// Benchmark pointer-based visitor
	measureTimeFor<void>("Benchmark.IterateAll.Pointer ", 
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
	measureTimeFor<void>("Benchmark.IterateAll.Address  ", 
		[&]() { 
			core::visitDepthFirst(core::ProgramAddress(program),
				core::makeLambdaVisitor([&](const NodeAddress& cur) { count++; }, true)
			);
		}
	);
	LOG(INFO) << "Number of nodes: " << count;

	// Benchmark empty-substitution operation
	count = 0;
	measureTimeFor<void>("Benchmark.IterateAll.Address  ", 
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
	measureTimeFor<void>("Benchmark.NodeSubstitution.Non-Types ", 
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

//***************************************************************************************
// Dump CFG
//***************************************************************************************
void dumpCFG(const NodePtr& program, const std::string& outFile) {
	if(outFile.empty()) { return; }

	utils::Timer timer();
	anal::CFGPtr graph = measureTimeFor<anal::CFGPtr>("Build.CFG", [&]() {
		return anal::CFG::buildCFG<anal::OneStmtPerBasicBlock>(program);
	});

	//measureTimeFor<void>( "DFA.ConstantPropagation", [&]() { 
			//anal::dfa::Solver<anal::dfa::analyses::ConstantPropagation> s(*graph);
			//s.solve();
		//});

	//measureTimeFor<void>( "DFA.ReachingDefinitions", [&]() { 
	//		anal::dfa::Solver<anal::dfa::analyses::ReachingDefinitions> s(*graph);
	//		s.solve();
	//	});

	int num = measureTimeFor<int>( "CFG.Strong.Components", [&]() { 
			return graph->getStrongComponents();
		});
	LOG(INFO) << "Number of connected components: " << num;

	measureTimeFor<void>( "Visit.CFG", [&]() { 
		std::fstream dotFile(outFile.c_str(), std::fstream::out | std::fstream::trunc);
		dotFile << *graph; 
		}
	);
}

//***************************************************************************************
// Test Stuff
// Function utilized to add temporary (non-clean) solution to the IR this is enabled only 
// when the --test flag is passed to the compiler
//***************************************************************************************
void testModule(const core::ProgramPtr& program, const CommandLineOptions& options) {
	if ( !options.Test ) { return; }

	// do nasty stuff
	anal::RefList&& refs = anal::collectDefUse(program);
	std::for_each(refs.begin(), refs.end(), [](const anal::RefPtr& cur){ 
		std::cout << *cur << std::endl; 
	});


	insieme::analysis::loadFunctionSemantics(program->getNodeManager());
	
	typedef std::vector<core::CallExprAddress> CallExprList;
	CallExprList mpiCalls;

	auto&& filter = [&] (const CallExprAddress& callExpr) -> bool { 
		static core::LiteralAddress lit;
		return (lit = dynamic_address_cast<const Literal>(callExpr->getFunctionExpr()) ) && 
			    lit->getStringValue().compare(0,4,"MPI_") == 0;
	};

	typedef void (CallExprList::*PushBackPtr)(const CallExprAddress&);

	PushBackPtr push_back = &CallExprList::push_back;
	visitDepthFirst( core::NodeAddress(program), core::makeLambdaVisitor( filter, fun(mpiCalls, push_back) ) );
	
	using namespace insieme::analysis;

	for_each(mpiCalls, [&](const CallExprAddress& cur) { 
			LOG(INFO) << *cur;
			core::LiteralPtr lit = core::static_pointer_cast<const Literal>(cur.getAddressedNode()->getFunctionExpr());
			LOG(INFO) << *lit->getType();
		} );

	

	//insieme::analysis::mpi::CommGraph&& g = insieme::analysis::mpi::extractCommGraph( program );
	//insieme::analysis::CFGPtr cfg = insieme::analysis::CFG::buildCFG<insieme::analysis::OneStmtPerBasicBlock>( program );

	//insieme::analysis::mpi::merge(cfg, g);

	//measureTimeFor<void>( "Visit.CFG", [&]() { 
	//	std::fstream dotFile("cfg.dot", std::fstream::out | std::fstream::trunc);
	//	dotFile << *cfg; 
	//	}
//	);

}

//***************************************************************************************
// IR Pretty Print 
//***************************************************************************************
typedef utils::map::PointerMap<core::NodePtr, core::printer::SourceRange> InverseStmtMap;

void createInvMap(const core::printer::SourceLocationMap& locMap, InverseStmtMap& invMap) {
	std::for_each(locMap.begin(), locMap.end(), 
		[&invMap](const insieme::core::printer::SourceLocationMap::value_type& cur) {
			invMap.insert( std::make_pair(cur.second, cur.first) );
		}
	);
}

void printIR(const NodePtr& program, InverseStmtMap& stmtMap, const CommandLineOptions& options) {
	using namespace insieme::core::printer;

	if ( !options.PrettyPrint && options.DumpIR.empty() ) { return; }

	// A pretty print of the AST
	measureTimeFor<void>("IR.PrettyPrint ", 
		[&]() {
			LOG(INFO) << "========================= Pretty Print INSPIRE ==================================";
			if(!options.DumpIR.empty()) {
				// write into the file
				std::fstream fout(options.DumpIR,  std::fstream::out | std::fstream::trunc);
				fout << "// -------------- Pretty Print Inspire --------------" << std::endl;
				fout << PrettyPrinter(program);
				fout << std::endl << std::endl << std::endl;
				fout << "// --------- Pretty Print Inspire - Detail ----------" << std::endl;
				fout << PrettyPrinter(program, PrettyPrinter::OPTIONS_MAX_DETAIL);
			} else {
				SourceLocationMap&& srcMap = 
					printAndMap( LOG_STREAM(INFO), 
						PrettyPrinter(program, PrettyPrinter::OPTIONS_DETAIL), 
						options.ShowLineNo, options.ColumnWrap
					);
				LOG(INFO) << "Number of generated source code mappings: " << srcMap.size();
				createInvMap(srcMap, stmtMap);
			}

		}
	);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}





//***************************************************************************************
// Check Semantics 
//***************************************************************************************
void checkSema(const core::NodePtr& program, MessageList& list, const InverseStmtMap& stmtMap, const CommandLineOptions& options) {

	using namespace insieme::core::printer;

	// Skip semantics checks if the flag is not set
	if (!options.CheckSema) { return; }

	LOG(INFO) << "=========================== IR Semantic Checks ==================================";
	insieme::utils::Timer timer("Checks");

	measureTimeFor<void>("Semantic Checks ", 
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

			auto fit = stmtMap.find(address.getAddressedNode());
			if (fit != stmtMap.end()) {
				LOG(INFO) << "Source Location: " << fit->second;
			}

		} while(ss.str().length() < MIN_CONTEXT && contextSize++ < 5);
//		LOG(INFO) << "\t Source-Node-Type: " << address->getNodeType();
		LOG(INFO) << "\t Source: " << PrettyPrinter(address, PrettyPrinter::OPTIONS_SINGLE_LINE);
		LOG(INFO) << "\t Context: " << ss.str() << std::endl;
//		LOG(INFO) << "\t All: " << PrettyPrinter(address.getRootNode());
	});

	// In the case of semantic errors, quit
	if ( !list.getErrors().empty() ) {
		cerr << "---- Semantic errors encountered - compilation aborted!! ----\n";
		exit(1);
	}

	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

//***************************************************************************************
// Mark SCoPs 
//***************************************************************************************
void markSCoPs(ProgramPtr& program, MessageList& errors, const InverseStmtMap& stmtMap, const CommandLineOptions& options) {
	if (!options.MarkScop) { return; }
	using namespace anal::polyhedral::scop;
	using namespace insieme::transform::pattern;
	using namespace insieme::transform::polyhedral;
	using insieme::transform::pattern::any;

	AddressList sl = measureTimeFor<AddressList>("IR.SCoP.Analysis ", 
		[&]() -> AddressList { return mark(program); });

	LOG(INFO) << "SCOP Analysis: " << sl.size() << std::endl;
	size_t numStmtsInScops = 0;
	size_t loopNests = 0, maxLoopNest=0;

	utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;
	std::for_each(sl.begin(), sl.end(),	[&](AddressList::value_type& cur){ 

		// performing dependence analysis
		//computeDataDependence(cur);

		// core::NodePtr ir = toIR(cur);
		// checkSema(ir, errors, stmtMap);
		// replacements.insert( std::make_pair(cur.getAddressedNode(), ir) );

		ScopRegion& reg = *cur->getAnnotation(ScopRegion::KEY);

		LOG(INFO) << reg.getScop();

		core::NodePtr ir = reg.getScop().toIR(program->getNodeManager());
		LOG(INFO) << *ir;

		LOG(INFO) << insieme::analysis::dep::extractDependenceGraph( cur.getAddressedNode(), 
			insieme::analysis::dep::RAW | insieme::analysis::dep::WAR | insieme::analysis::dep::WAW
		);

		numStmtsInScops += reg.getScop().size();
		size_t loopNest = reg.getScop().nestingLevel();
		
		if( loopNest > maxLoopNest) { maxLoopNest = loopNest; }
		loopNests += loopNest;
	});	

	LOG(INFO) << std::setfill(' ') << std::endl
		  << "=========================================" << std::endl
		  << "=             SCoP COVERAGE             =" << std::endl
		  << "=~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~=" << std::endl
		  << "= Tot # of SCoPs                :" << std::setw(6) 
				<< std::right << sl.size() << " =" << std::endl
		  << "= Tot # of stms covered by SCoPs:" << std::setw(6) 
				<< std::right << numStmtsInScops << " =" << std::endl
		  << "= Avg stmt per SCoP             :" << std::setw(6) 
				<< std::setprecision(4) << std::right 
				<< (double)numStmtsInScops/sl.size() << " =" << std::endl
		  << "= Avg loop nests per SCoP       :" << std::setw(6) 
				<< std::setprecision(4) << std::right 
				<< (double)loopNests/sl.size() << " =" << std::endl
		  << "= Max loop nests per SCoP       :" << std::setw(6) 
				<< std::setprecision(4) << std::right 
				<< maxLoopNest << " =" << std::endl
		  << "=========================================";
}

//***************************************************************************************
// Dump IR 
//***************************************************************************************
void showIR(const core::ProgramPtr& program, MessageList& errors, const CommandLineOptions& options) {
	// Creates dot graph of the generated IR
	if(options.ShowIR.empty()) { return; }
	measureTimeFor<void>("Show.graph", 
		[&]() {
			std::fstream dotOut(options.ShowIR.c_str(), std::fstream::out | std::fstream::trunc);
			insieme::driver::printer::printDotGraph(program, errors, dotOut);
		} 
	);
}

void applyOpenMPFrontend(core::ProgramPtr& program, const CommandLineOptions& options) {
	if (!options.OpenMP) { return; }

	LOG(INFO) << "============================= OMP conversion ====================================";
	program = measureTimeFor<core::ProgramPtr>("OpenMP ",
			[&]() {return fe::omp::applySema(program, program->getNodeManager()); }
		);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void applyOpenCLFrontend(core::ProgramPtr& program, const CommandLineOptions& options) {
	if (!options.OpenCL) { return; }

	LOG(INFO) << "============================= OpenCL conversion ====================================";
	fe::ocl::HostCompiler oclHostCompiler(program, options);
	program = measureTimeFor<core::ProgramPtr>("OpenCL ",
			[&]() {return oclHostCompiler.compile(); }
		);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void applyCilkFrontend(core::ProgramPtr& program, const CommandLineOptions& options) {
	if (!options.Cilk) { return; }

	LOG(INFO) << "============================= Cilk conversion ====================================";
	program = measureTimeFor<core::ProgramPtr>("Cilk ",
			[&]() {return fe::cilk::applySema(program, program->getNodeManager()); }
		);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void showStatistics(const core::ProgramPtr& program, const CommandLineOptions& options) {
	if (!options.ShowStats) { return; }

	LOG(INFO) << "============================ IR Statistics ======================================";
	measureTimeFor<void>("ir.statistics ", [&]() {
		LOG(INFO) << "\n" << IRStatistic::evaluate(program);
	});
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void doCleanup(core::ProgramPtr& program, const CommandLineOptions& options) {
	// if (!options.Cleanup) { return; }

	LOG(INFO) << "================================ IR CLEANUP =====================================";
	program = measureTimeFor<core::ProgramPtr>("ir.cleanup", [&]() {
		 return static_pointer_cast<const core::Program>( insieme::transform::cleanup(program, options.ConstantPropagation) );
	} );
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

	// IR statistics
	showStatistics(program, options);
}

//***************************************************************************************
// Feature Extractor
//***************************************************************************************
void featureExtract(const core::ProgramPtr& program, const CommandLineOptions& options) {
	if (!options.FeatureExtract) { return; }
	LOG(INFO) << "Feature extract mode";
	// anal::collectFeatures(program); -- seriously, this wasn't doing a shit!!
	return;
}

} // end anonymous namespace 


/** 
 * Insieme compiler entry point 
 */
int main(int argc, char** argv) {

	const CommandLineOptions options = CommandLineOptions::parse(argc, argv);
	if (!options.valid) return 0;		// it was a help or about request

	Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr(options.LogLevel), options.Verbosity);
	LOG(INFO) << "Insieme compiler - Version: " << utils::getVersion();

	core::NodeManager manager;
	core::ProgramPtr program = core::Program::get(manager);
	try {
		if(!options.InputFiles.empty()) {
			auto inputFiles = options.InputFiles;
			// LOG(INFO) << "Parsing input files: ";
			// std::copy(inputFiles.begin(), inputFiles.end(), std::ostream_iterator<std::string>( std::cout, ", " ) );
			fe::Program p(manager, options);
			
			measureTimeFor<void>("Frontend.load [clang]", [&]() { p.addTranslationUnits(options); } );

			// do the actual clang to IR conversion
			program = measureTimeFor<core::ProgramPtr>("Frontend.convert ", [&]() { return p.convert(); } );

			InverseStmtMap stmtMap;

			printIR(program, stmtMap, options);

			doCleanup(program, options);

			// run OpenCL frontend
			applyOpenCLFrontend(program, options);

			// Load known function semantics from the function database
			anal::loadFunctionSemantics(program->getNodeManager());

			// Check for annotations on IR nodes relative to transformations which should be applied, and applies them.
			program = measureTimeFor<ProgramPtr>("Pragma.Transformer", 
					[&]() { return insieme::driver::pragma::applyTransfomrations(program); } );

			// Handling of pragma info
			program = measureTimeFor<ProgramPtr>("Pragma.Info",  
					[&]() { return insieme::driver::pragma::handlePragmaInfo(program, options); } );

			printIR(program, stmtMap, options);

			// perform checks
			MessageList errors;
			if(options.CheckSema) {	checkSema(program, errors, stmtMap, options);	}

			printIR(program, stmtMap, options);

			// run OMP frontend
			if(options.OpenMP) {
				stmtMap.clear();
				applyOpenMPFrontend(program, options);
				printIR(program, stmtMap, options);
				// check again if the OMP flag is on
				if(options.CheckSema) { checkSema(program, errors, stmtMap, options); }
			}

			// run Cilk frontend
			if(options.Cilk) {
				applyCilkFrontend(program, options);
				// check again if the OMP flag is on
				printIR(program, stmtMap, options);
				if(options.CheckSema) { checkSema(program, errors, stmtMap, options); }
			}


			/**************######################################################################################################***/

			//cout << "\n\n******************************************************* REGIONS \n\n";
			//for_each(regions, [](const NodeAddress& a) {
			//	cout << "\n***** REGION \n";
			//	cout << printer::PrettyPrinter(a.getAddressedNode());
			//});
			/**************######################################################################################################***/

			// This function is a hook useful when some hack needs to be tested
			testModule(program, options);

			// Performs some benchmarks 
			doBenchmarkCore(manager, program, options);
		
			// Dump the Inter procedural Control Flow Graph associated to this program
			dumpCFG(program, options.CFG);

			//printIR(program, stmtMap);

			// Perform SCoP region analysis 
			markSCoPs(program, errors, stmtMap, options);
			// printIR(program, stmtMap);
			
			if(options.CheckSema) {	checkSema(program, errors, stmtMap, options); }

			// IR statistics
			showStatistics(program, options);

			// Creates dot graph of the generated IR
			showIR(program, errors, options);

			#ifdef USE_XML
			// XML dump
			if(!options.DumpXML.empty()) {
				LOG(INFO) << "================================== XML DUMP =====================================";
				measureTimeFor<void>("Xml.dump ", 
						[&]() { xml::XmlUtil::write(program, options.DumpXML); }
					);
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			}
			#endif
		
			// do some cleanup 
			//doCleanup(program);
			//printIR(program, stmtMap);
			//if (options.Cleanup) { checkSema(program, errors, stmtMap); }

			// Extract features
			if (options.FeatureExtract) { featureExtract(program, options); }
		}

		#ifdef USE_XML
		if(!options.LoadXML.empty()) {
			LOG(INFO) << "================================== XML LOAD =====================================";
			insieme::utils::Timer timer("Xml.load");
			NodePtr&& xmlNode= xml::XmlUtil::read(manager, options.LoadXML);
			program = core::dynamic_pointer_cast<const Program>(xmlNode);
			assert(program && "Loaded XML doesn't represent a valid program");
			timer.stop();
			LOG(INFO) << timer;
			LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
		}
		#endif

//		{
//			LOG(INFO) << "================================== Checking Hashes =====================================";
//			insieme::utils::Timer timer("hashes");
//			checkForHashCollisions(program);
//			timer.stop();
//			LOG(INFO) << timer;
//			LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
//		}


		{
			string backendName = "";
			be::BackendPtr backend;

			// see whether a backend has been selected
			if (!options.Backend.empty()) {

				// get option
				char selection = options.Backend[0];
				if (selection < 'a') { // to lower case
					selection += 'a' - 'A';
				}

				// ###################################################
				// TODO: remove this
				// enforces the usage of the full backend for testing
//				selection = 'r';
//				selection = 's';
//				selection = 'o';
				// ###################################################


				switch(selection) {
					case 'o': {
						// check if the host is in the entrypoints, otherwise use the kernel backend
						bool host = false;
						const vector<ExpressionPtr>& ep = program->getEntryPoints();
						for (vector<ExpressionPtr>::const_iterator it = ep.begin(); it != ep.end(); ++it) {
							if((*it)->hasAnnotation(BaseAnnotation::KEY)) {
								BaseAnnotationPtr&& annotations = (*it)->getAnnotation(BaseAnnotation::KEY);
								for(BaseAnnotation::AnnotationList::const_iterator iter = annotations->getAnnotationListBegin();
									iter < annotations->getAnnotationListEnd(); ++iter) {
									if(!dynamic_pointer_cast<KernelFctAnnotation>(*iter)) {
										host = true;
									}
								}
							} else
								host = true;
						}

						// check if a path to dump the binary representation of the kernel is passed (form: -b ocl:PATH)
						std::string kernelDumpPath;
						size_t idx = options.Backend.find(":");
						if(idx != std::string::npos) {
							kernelDumpPath = options.Backend.substr(idx+1, options.Backend.size());
//							std::cout << idx << " hallo " << binaryDumpPath << std::endl;
						}

						if (host) {
							backendName = "OpenCL.Host.Backend";
							backend = insieme::backend::ocl_host::OCLHostBackend::getDefault(kernelDumpPath);
						} else {
							backendName = "OpenCL.Kernel.Backend";
							backend = insieme::backend::ocl_kernel::OCLKernelBackend::getDefault(kernelDumpPath);
						}
						break;
					}
					case 's': {
						backendName = "Sequential.Backend";
						backend = insieme::backend::sequential::SequentialBackend::getDefault();
						break;
					}
					case 'r':
					default: {
						backendName = "Runtime.Backend";
						backend = insieme::backend::runtime::RuntimeBackend::getDefault(options.EstimateEffort);
						break;
					}
				}

				insieme::utils::Timer timer(backendName);

				LOG(INFO) << "======================= Converting to TargetCode ================================";

				// convert code
				be::TargetCodePtr targetCode = backend->convert(program);
				
				// select output target
				if(!options.Output.empty()) {
					// write result to file ...
					std::fstream outFile(options.Output, std::fstream::out | std::fstream::trunc);
					outFile << *targetCode;
					outFile.close();

					// TODO: reinstate rewriter when fractions of programs are supported as entry points
//					insieme::backend::Rewriter::writeBack(program, insieme::simple_backend::convert(program), options.Output);

				} else {
					// just write result to logger
					LOG(INFO) << *targetCode;
				}

				// print timing information
				timer.stop();
				LOG(INFO) << timer;
			}

		}

	} catch (fe::ClangParsingError& e) {
		cerr << "Error while parsing input file: " << e.what() << endl;
		exit(1);
	}
}

// ------------------------------------------------------------------------------------------------------------------
//                                     Hash code evaluation
// ------------------------------------------------------------------------------------------------------------------


bool checkForHashCollisions(const ProgramPtr& program) {

	// create a set of all nodes
	insieme::utils::set::PointerSet<NodePtr> allNodes;
	insieme::core::visitDepthFirstOnce(program, insieme::core::makeLambdaVisitor([&allNodes](const NodePtr& cur) {
		allNodes.insert(cur);
	}, true));

	// evaluate hash codes
	LOG(INFO) << "Number of nodes: " << allNodes.size();
	std::map<std::size_t, NodePtr> hashIndex;
	int collisionCount = 0;
	for_each(allNodes, [&](const NodePtr& cur) {
		// try inserting node
		std::size_t hash = (*cur).hash();
		//std::size_t hash = boost::hash_value(cur->toString());
		//std::size_t hash = ::computeHash(cur);

		auto res = hashIndex.insert(std::make_pair(hash, cur));
		if (!res.second) {
			LOG(INFO) << "Hash Collision detected: \n"
					  << "   Hash code:     " << hash << "\n"
					  << "   First Element: " << *res.first->second << "\n"
					  << "   New Element:   " << *cur << "\n"
					  << "   Equal:         " << ((*cur==*res.first->second)?"true":"false") << "\n";
			collisionCount++;
		}
	});
	LOG(INFO) << "Number of Collisions: " << collisionCount;

	// terminate main program
	return false;

}
