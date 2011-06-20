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
#include "insieme/backend/full_backend.h"
#include "insieme/simple_backend/rewrite.h"

#include "insieme/analysis/cfg.h"

#include "insieme/transform/ir_cleanup.h"

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"
#include "insieme/utils/map_utils.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/omp/omp_sema.h"

#include "insieme/driver/dot_printer.h"

#ifdef USE_XML
#include "insieme/xml/xml_utils.h"
#endif

#include "insieme/analysis/scop.h"

using namespace std;
using namespace insieme::utils::log;

namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace be = insieme::backend;
namespace xml = insieme::xml;
namespace utils = insieme::utils;
namespace analysis = insieme::analysis;

bool checkForHashCollisions(const ProgramPtr& program);

typedef utils::map::PointerMap<core::NodePtr, insieme::core::printer::SourceRange> InverseStmtMap;

void createInvMap(const insieme::core::printer::SourceLocationMap& locMap, InverseStmtMap& invMap) {
	std::for_each(locMap.begin(), locMap.end(), 
		[&invMap](const insieme::core::printer::SourceLocationMap::value_type& cur) {
			invMap.insert( std::make_pair(cur.second, cur.first) );
		}
	);
}

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
			insieme::utils::Timer clangTimer("Frontend.load [clang]");
			p.addTranslationUnits(inputFiles);
			clangTimer.stop();
			LOG(INFO) << clangTimer;
			
			// do the actual clang to IR conversion
			insieme::utils::Timer convertTimer("Frontend.convert ");
			program = p.convert();
			convertTimer.stop();
			LOG(INFO) << convertTimer;

			// perform some performance benchmarks
			if (CommandLineOptions::BenchmarkCore) {

				// Benchmark pointer-based visitor
				insieme::utils::Timer visitPtrTime("Benchmark.IterateAll.Pointer ");
				int count = 0;
				core::visitAll(program, core::makeLambdaVisitor([&](const NodePtr& cur) {
					count++;
				}, true));
				visitPtrTime.stop();
				LOG(INFO) << visitPtrTime;
				LOG(INFO) << "Number of nodes: " << count;

				// Benchmark address based visitor
				insieme::utils::Timer visitAddrTime("Benchmark.IterateAll.Address ");
				count = 0;
				core::visitAll(core::ProgramAddress(program), core::makeLambdaVisitor([&](const NodeAddress& cur) {
					count++;
				}, true));
				visitAddrTime.stop();
				LOG(INFO) << visitAddrTime;
				LOG(INFO) << "Number of nodes: " << count;

				// Benchmark empty-substitution operation
				insieme::utils::Timer substitutionTime("Benchmark.NodeSubstitution ");
				count = 0;
				NodeMapping* h;
				auto mapper = makeLambdaMapper([&](unsigned, const NodePtr& cur)->NodePtr {
					count++;
					return cur->substitute(manager, *h);
				});
				h = &mapper;
				mapper.map(0,program);
				substitutionTime.stop();
				LOG(INFO) << substitutionTime;
				LOG(INFO) << "Number of modifications: " << count;

				// Benchmark empty-substitution operation (non-types only)
				insieme::utils::Timer substitutionTime2("Benchmark.NodeSubstitution.Non-Types ");
				count = 0;
				NodeMapping* h2;
				auto mapper2 = makeLambdaMapper([&](unsigned, const NodePtr& cur)->NodePtr {
					if (cur->getNodeCategory() == NC_Type) {
						return cur;
					}
					count++;
					return cur->substitute(manager, *h2);
				});
				h2 = &mapper2;
				mapper2.map(0,program);
				substitutionTime2.stop();
				LOG(INFO) << substitutionTime2;
				LOG(INFO) << "Number of modifications: " << count;
			}

			// Dump CFG
			if(!CommandLineOptions::CFG.empty()) {
				insieme::utils::Timer timer("Build.CFG");
				std::fstream dotFile(CommandLineOptions::CFG.c_str(), std::fstream::out | std::fstream::trunc);
				analysis::CFGPtr graph = analysis::CFG::buildCFG<analysis::OneStmtPerBasicBlock>(program);
				timer.stop();
				LOG(INFO) << timer;
				timer = insieme::utils::Timer("Visit.CFG");
				dotFile << *graph;
				timer.stop();
				LOG(INFO) << timer;
			}
		
			insieme::analysis::scop::ScopList sl = insieme::analysis::scop::mark(program);
			std::cout << "SCOP Analysis: " << sl.size() << std::endl;

			InverseStmtMap stmtMap;
			if(CommandLineOptions::PrettyPrint || !CommandLineOptions::DumpIR.empty()) {
				using namespace insieme::core::printer;
				// a pretty print of the AST
				insieme::utils::Timer timer("IR.PrettyPrint");
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
					SourceLocationMap srcMap = printAndMap(LOG_STREAM(INFO), PrettyPrinter(program, PrettyPrinter::OPTIONS_DETAIL), 
														   CommandLineOptions::ShowLineNo, CommandLineOptions::ColumnWrap
														  );
					LOG(INFO) << "Number of generated source code mappings: " << srcMap.size();
					createInvMap(srcMap, stmtMap);
				}

				timer.stop();
				LOG(INFO) << timer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

				// LOG(INFO) << "====================== Pretty Print INSPIRE Detail ==============================";
				// LOG(INFO) << insieme::core::printer::PrettyPrinter(program, insieme::core::printer::PrettyPrinter::OPTIONS_DETAIL);
				// LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			}

			// perform checks
			MessageList errors;
			auto checker = [&]() {
				using namespace insieme::core::printer;
				LOG(INFO) << "=========================== IR Semantic Checks ==================================";
				insieme::utils::Timer timer("Checks");
				errors = check(program, insieme::core::checks::getFullCheck());
				std::sort(errors.begin(), errors.end());
				for_each(errors, [&](const Message& cur) {
					LOG(INFO) << cur;
					NodeAddress address = cur.getAddress();
					stringstream ss;
					unsigned contextSize = 1;
					do {
						ss.str(""); 
						ss.clear();
						NodePtr context = address.getParentNode(min((unsigned)contextSize, address.getDepth()-contextSize));
						ss << PrettyPrinter(context, PrettyPrinter::OPTIONS_SINGLE_LINE, 1+2*contextSize);
						LOG(INFO) << "Source Location: " << stmtMap[address.getAddressedNode()];

					} while(ss.str().length() < MIN_CONTEXT && contextSize++ < 5);
					LOG(INFO) << "\t Context: " << ss.str() << std::endl;
				});

				if ( !errors.empty() ) {
					exit(1);
				}
				timer.stop();
				LOG(INFO) << timer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			};

//			if(CommandLineOptions::CheckSema) {
//				checker();
//			}

			// run OMP frontend
			if(CommandLineOptions::OMPSema) {
				LOG(INFO) << "============================= OMP conversion ====================================";
				insieme::utils::Timer ompTimer("OMP");
				program = fe::omp::applySema(program,  manager);
				ompTimer.stop();
				LOG(INFO) << ompTimer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
				// check again
				if(CommandLineOptions::CheckSema) {
					checker();
				}
			}

			// IR statistics
			if (CommandLineOptions::ShowStats) {
				LOG(INFO) << "============================ IR Statistics ======================================";
				insieme::utils::Timer statTimer("Statistics");
				LOG(INFO) << "\n" << ASTStatistic::evaluate(program);
				statTimer.stop();
				LOG(INFO) << statTimer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			}

			// Creates dot graph of the generated IR
			if(!CommandLineOptions::ShowIR.empty()) {
				insieme::utils::Timer timer("Show.graph");
				std::fstream dotFile(CommandLineOptions::ShowIR.c_str(), std::fstream::out | std::fstream::trunc);
				insieme::driver::printDotGraph(program, errors, dotFile);
				timer.stop();
				LOG(INFO) << timer;
			}

			#ifdef USE_XML
			// XML dump
			if(!CommandLineOptions::DumpXML.empty()) {
				LOG(INFO) << "================================== XML DUMP =====================================";
				insieme::utils::Timer timer("Xml.dump");
				xml::XmlUtil::write(program, CommandLineOptions::DumpXML);
				timer.stop();
				LOG(INFO) << timer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			}
			#endif

			// do some cleanup
			if (CommandLineOptions::Cleanup) {
				LOG(INFO) << "================================ IR CLEANUP =====================================";
				insieme::utils::Timer timer("ir.cleanup");
				program = static_pointer_cast<const core::Program>(insieme::transform::cleanup(program));
				timer.stop();
				LOG(INFO) << timer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

				// IR statistics
				LOG(INFO) << "============================ IR Statistics ======================================";
				LOG(INFO) << "\n" << ASTStatistic::evaluate(program);
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

				// check again
				LOG(INFO) << "============================= Checking IR =======================================";
				if(CommandLineOptions::CheckSema) {
					checker();
				}
			}

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
			if (CommandLineOptions::OpenCL) {
				backendName = "OpenCL.Backend";

//TODO find the OpenCLChecker
//				insieme::opencl_backend::OpenCLChecker oc;
//				LOG(INFO) << "Checking OpenCL compatibility ... " << (oc.check(program) ? "OK" : "ERROR\nInput program cannot be translated to OpenCL!");

				// obtain open CL backend instance
				backend = insieme::backend::ocl::OpenCLBackend::getDefault();
			} else if (CommandLineOptions::OmegaBackend) {
				backendName = "Full.Backend";
				backend = insieme::backend::FullBackend::getDefault();
			} else {
				backendName = "Simple.Backend";
				backend = insieme::simple_backend::SimpleBackend::getDefault();
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
//				insieme::backend::Rewriter::writeBack(program, insieme::simple_backend::convert(program), CommandLineOptions::Output);

			} else {
				// just write result to logger
				LOG(INFO) << *targetCode;
			}

			// print timing information
			timer.stop();
			LOG(INFO) << timer;
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
	insieme::core::visitAllOnce(program, insieme::core::makeLambdaVisitor([&allNodes](const NodePtr& cur) {
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
