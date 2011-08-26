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

#define MIN_CONTEXT 40

#include "insieme/core/ast_statistic.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/backend.h"

#include "insieme/simple_backend/simple_backend.h"
#include "insieme/opencl_backend/opencl_convert.h"
#include "insieme/simple_backend/rewrite.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/ocl_kernel/kernel_backend.h"
#include "insieme/backend/ocl_host/host_backend.h"

#include "insieme/transform/ir_cleanup.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/map_utils.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/driver/dot_printer.h"

#ifdef USE_XML
#include "insieme/xml/xml_utils.h"
#endif

#include "insieme/analysis/cfg.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/defuse_collect.h"
#include "insieme/analysis/features_collect.h"

using namespace std;
using namespace insieme::utils::log;
using namespace insieme::annotations::ocl;

namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace be = insieme::backend;
namespace xml = insieme::xml;
namespace utils = insieme::utils;
namespace analysis = insieme::analysis;

bool checkForHashCollisions(const ProgramPtr& program);

namespace {

template <class Ret=void>
Ret measureTimeFor(const std::string& timerName, const std::function<Ret ()>& task) {
	utils::Timer timer(timerName);
	Ret ret = task(); // execute the job
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
void doBenchmarkCore(NodeManager& mgr, const NodePtr& program) {

	if (!CommandLineOptions::BenchmarkCore) return;

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
	analysis::CFGPtr graph = measureTimeFor<analysis::CFGPtr>("Build.CFG", [&]() {
		return analysis::CFG::buildCFG<analysis::OneStmtPerBasicBlock>(program);
	});
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
void testModule(const core::ProgramPtr& program) {
	if ( !CommandLineOptions::Test ) { return; }

	// do nasty stuff
	analysis::RefList&& refs = analysis::collectDefUse(program);
	std::for_each(refs.begin(), refs.end(), [](const analysis::RefPtr& cur){ 
		std::cout << *cur << std::endl; 
	});
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

void printIR(const NodePtr& program, InverseStmtMap& stmtMap) {
	using namespace insieme::core::printer;

	if ( !CommandLineOptions::PrettyPrint && CommandLineOptions::DumpIR.empty() ) { return; }

	// A pretty print of the AST
	measureTimeFor<void>("IR.PrettyPrint ", 
		[&]() {
			LOG(INFO) << "========================= Pretty Print INSPIRE ==================================";
			if(!CommandLineOptions::DumpIR.empty()) {
				// write into the file
				std::fstream fout(CommandLineOptions::DumpIR,  std::fstream::out | std::fstream::trunc);
				fout << "// -------------- Pretty Print Inspire --------------" << std::endl;
				fout << PrettyPrinter(program);
				fout << std::endl << std::endl << std::endl;
				fout << "// --------- Pretty Print Inspire - Detail ----------" << std::endl;
				fout << PrettyPrinter(program, PrettyPrinter::OPTIONS_DETAIL);
			} else {
				SourceLocationMap&& srcMap = 
					printAndMap( LOG_STREAM(INFO), 
						PrettyPrinter(program, PrettyPrinter::OPTIONS_DETAIL), 
						CommandLineOptions::ShowLineNo, CommandLineOptions::ColumnWrap 
					);
				LOG(INFO) << "Number of generated source code mappings: " << srcMap.size();
				createInvMap(srcMap, stmtMap);
			}

		}
	);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

	// LOG(INFO) << "====================== Pretty Print INSPIRE Detail ==============================";
	// LOG(INFO) << insieme::core::printer::PrettyPrinter(program, insieme::core::printer::PrettyPrinter::OPTIONS_DETAIL);
	// LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

//***************************************************************************************
// Mark SCoPs 
//***************************************************************************************
void markSCoPs(const ProgramPtr& program) {
	if (!CommandLineOptions::MarkScop) { return; }
	using namespace insieme::analysis::scop;

	AddressList sl = measureTimeFor<AddressList>("IR.SCoP.Analysis ", 
		[&]() -> AddressList { return mark(program); });

	LOG(INFO) << "SCOP Analysis: " << sl.size() << std::endl;
	std::for_each(sl.begin(), sl.end(),	[](AddressList::value_type& cur){ 
			printSCoP(LOG_STREAM(INFO), cur); 
			// performing dependence analysis
			computeDataDependence(cur);
		}
	);	
}

//***************************************************************************************
// Check Semantics 
//***************************************************************************************
void checkSema(const core::ProgramPtr& program, MessageList& list, const InverseStmtMap& stmtMap) {
	using namespace insieme::core::printer;

	// Skip semantics checks if the flag is not set
	if (!CommandLineOptions::CheckSema) { return; }

	LOG(INFO) << "=========================== IR Semantic Checks ==================================";
	insieme::utils::Timer timer("Checks");

	measureTimeFor<void>("Semantic Checks ", 
		[&]() { list = check( program, core::checks::getFullCheck() ); }
	);

	auto errors = list.getAll();
	std::sort(errors.begin(), errors.end());
	for_each(errors, [&](const Message& cur) {
		LOG(INFO) << cur;
		NodeAddress address = cur.getAddress();
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
		LOG(INFO) << "\t Context: " << ss.str() << std::endl;
	});

	// In the case of semantic errors, quit
	if ( !list.getErrors().empty() ) {
		cerr << "---- Semantic errors encountered - compilation aborted!! ----\n";
		exit(1);
	}

	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

//***************************************************************************************
// Dump IR 
//***************************************************************************************
void showIR(const core::ProgramPtr& program, MessageList& errors) {
	// Creates dot graph of the generated IR
	if(CommandLineOptions::ShowIR.empty()) { return; }
	measureTimeFor<void>("Show.graph", 
		[&]() {
			std::fstream dotOut(CommandLineOptions::ShowIR.c_str(), std::fstream::out | std::fstream::trunc);
			insieme::driver::printDotGraph(program, errors, dotOut);
		} 
	);
}

void applyOpenMPFrontend(core::ProgramPtr& program) {
	if (!CommandLineOptions::OpenMP) { return; }
	
	LOG(INFO) << "============================= OMP conversion ====================================";
	program = measureTimeFor<core::ProgramPtr>("OpenMP ",
			[&]() {return fe::omp::applySema(program, program->getNodeManager()); }
		);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void applyOpenCLFrontend(core::ProgramPtr& program) {
	if (!CommandLineOptions::OpenCL) { return; }

	LOG(INFO) << "============================= OpenCL conversion ====================================";
	fe::ocl::HostCompiler oclHostCompiler(program);
	program = measureTimeFor<core::ProgramPtr>("OpenCL ",
			[&]() {return oclHostCompiler.compile(); }
		);
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void showStatistics(const core::ProgramPtr& program) {
	if (!CommandLineOptions::ShowStats) { return; }

	LOG(INFO) << "============================ IR Statistics ======================================";
	measureTimeFor<void>("ir.statistics ", [&]() {
		LOG(INFO) << "\n" << ASTStatistic::evaluate(program);
	});
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
}

void doCleanup(core::ProgramPtr& program) {
	if (!CommandLineOptions::Cleanup) { return; }

	LOG(INFO) << "================================ IR CLEANUP =====================================";
	program = measureTimeFor<core::ProgramPtr>("ir.cleanup", [&]() {
		 return static_pointer_cast<const core::Program>( insieme::transform::cleanup(program) );
	} );
	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

	// IR statistics
	showStatistics(program);
}

//***************************************************************************************
// Feature Extractor
//***************************************************************************************
void featureExtract(const core::ProgramPtr& program) {
	if (!CommandLineOptions::FeatureExtract) { return; }
	LOG(INFO) << "Feature extract mode";
	analysis::collectFeatures(program);
	return;
}

} // end anonymous namespace 

/** 
 * Insieme compiler entry point 
 */
int main(int argc, char** argv) {

	CommandLineOptions::Parse(argc, argv);
	Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr(CommandLineOptions::LogLevel));
	LOG(INFO) << "Insieme compiler";

	core::NodeManager manager;
	core::ProgramPtr program = core::Program::create(manager);
	try {
		if(!CommandLineOptions::InputFiles.empty()) {
			auto inputFiles = CommandLineOptions::InputFiles;
			// LOG(INFO) << "Parsing input files: ";
			// std::copy(inputFiles.begin(), inputFiles.end(), std::ostream_iterator<std::string>( std::cout, ", " ) );
			fe::Program p(manager);
			
			measureTimeFor<void>("Frontend.load [clang]", [&]() { p.addTranslationUnits(inputFiles); } );
			
			// do the actual clang to IR conversion
			program = measureTimeFor<core::ProgramPtr>("Frontend.convert ", [&]() { return p.convert(); } );

			// This function is a hook useful when some hack needs to be tested
			testModule(program);

			// Performs some benchmarks 
			doBenchmarkCore(manager, program);
		
			// Dump the Inter procedural Control Flow Graph associated to this program
			dumpCFG(program, CommandLineOptions::CFG);
			
			InverseStmtMap stmtMap;
			printIR(program, stmtMap);

			// run OpenCL frontend
			applyOpenCLFrontend(program);

			// perform checks
			MessageList errors;
			if(CommandLineOptions::CheckSema) {	checkSema(program, errors, stmtMap);	}

			// Perform SCoP region analysis 
			markSCoPs(program);

			// run OMP frontend
			applyOpenMPFrontend(program);
			// check again if the OMP flag is on
			if (CommandLineOptions::OpenMP) { checkSema(program, errors, stmtMap); }
			
			// IR statistics
			showStatistics(program);

			// Creates dot graph of the generated IR
			showIR(program, errors);

			#ifdef USE_XML
			// XML dump
			if(!CommandLineOptions::DumpXML.empty()) {
				LOG(INFO) << "================================== XML DUMP =====================================";
				measureTimeFor<void>("Xml.dump ", 
						[&]() { xml::XmlUtil::write(program, CommandLineOptions::DumpXML); }
					);
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			}
			#endif
		
			// do some cleanup 
			doCleanup(program);
			if (CommandLineOptions::Cleanup) { checkSema(program, errors, stmtMap); }

			// Extract features
			if (CommandLineOptions::FeatureExtract) { featureExtract(program); }
		}

		#ifdef USE_XML
		if(!CommandLineOptions::LoadXML.empty()) {
			LOG(INFO) << "================================== XML LOAD =====================================";
			insieme::utils::Timer timer("Xml.load");
			NodePtr&& xmlNode= xml::XmlUtil::read(manager, CommandLineOptions::LoadXML);
			program = core::dynamic_pointer_cast<const Program>(xmlNode);
			assert(program && "Loaded XML doesn't represent a valid program");
			timer.stop();
			LOG(INFO) << timer;
			LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
		}
		#endif

		{
			string backendName = "";
			be::BackendPtr backend;

			// see whether a backend has been selected
			if (!CommandLineOptions::Backend.empty()) {

				// get option
				char selection = CommandLineOptions::Backend[0];
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
std::cout << "Number of entry points: " << ep.size() << std::endl;
										host = true;
									}
								}
							} else
								host = true;
						}

						if (host) {
							backendName = "OpenCL.Host.Backend";
std::cout << "Running Host\n";
							backend = insieme::backend::ocl_host::OCLHostBackend::getDefault();
						} else {
							backendName = "OpenCL.Kernel.Backend";
std::cout << "Running kernel\n";
							backend = insieme::backend::ocl_kernel::OCLKernelBackend::getDefault();
						}
						break;
					}
					case 'r': {
						backendName = "Runtime.Backend";
						backend = insieme::backend::runtime::RuntimeBackend::getDefault();
						break;
					}
					case 's': {
						backendName = "Sequential.Backend";
						backend = insieme::backend::sequential::SequentialBackend::getDefault();
						break;
					}
					case 'p':
					default: {
						backendName = "Simple.Backend";
						backend = insieme::simple_backend::SimpleBackend::getDefault();
					}
				}

				insieme::utils::Timer timer(backendName);

				LOG(INFO) << "======================= Converting to TargetCode ================================";

				// convert code
				be::TargetCodePtr targetCode = backend->convert(program);

				// select output target
				if(!CommandLineOptions::Output.empty()) {
					// write result to file ...
					std::fstream outFile(CommandLineOptions::Output, std::fstream::out | std::fstream::trunc);
					outFile << *targetCode;
					outFile.close();

					// TODO: reinstate rewriter when fractions of programs are supported as entry points
//					insieme::backend::Rewriter::writeBack(program, insieme::simple_backend::convert(program), CommandLineOptions::Output);

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
		std::size_t hash = cur->hash();
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
