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

#include "insieme/simple_backend/backend_convert.h"
#include "insieme/simple_backend/rewrite.h"

// #include "insieme/opencl_backend/opencl_convert.h"
// #include "insieme/opencl_backend/opencl_checker.h"

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/omp/omp_sema.h"

#include "insieme/driver/dot_printer.h"

#include "insieme/xml/xml_utils.h"

using namespace std;
namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace xml = insieme::xml;

int main(int argc, char** argv) {

	CommandLineOptions::Parse(argc, argv);
	insieme::utils::InitLogger(argv[0], INFO, true);

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

			// perform checks
			MessageList errors;
			auto checker = [&]() {
				LOG(INFO) << "=========================== IR Semantic Checks ==================================";
				insieme::utils::Timer timer("Checks");
				errors = check(program, insieme::core::checks::getFullCheck());
				std::sort(errors.begin(), errors.end());
				for_each(errors, [](const Message& cur) {
					LOG(INFO) << cur << std::endl;
					NodeAddress address = cur.getAddress();
					stringstream ss;
					unsigned contextSize = 1;
					do {
						ss.str(""); 
						ss.clear();
						NodePtr context = address.getParentNode(min((unsigned)contextSize, address.getDepth()-contextSize));
						ss << insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 1+2*contextSize);
					} while(ss.str().length() < MIN_CONTEXT && contextSize++ < 5);
					LOG(INFO) << "\t Context: " << ss.str();
				});
				timer.stop();
				LOG(INFO) << timer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			};

			if(CommandLineOptions::CheckSema) {
				checker();
			}

			// run OMP frontend
			if(CommandLineOptions::OMPSema) {
				LOG(INFO) << "============================= OMP conversion ====================================";
				insieme::utils::Timer ompTimer("OMP");
				program = fe::omp::applySema(program,  manager);
				ompTimer.stop();
				LOG(INFO) << ompTimer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
				cout << core::printer::PrettyPrinter(program);
				// check again
				if(CommandLineOptions::CheckSema) {
					checker();
				}
			}

			// IR statistics
			LOG(INFO) << "============================ IR Statistics ======================================";
			LOG(INFO) << "\n" << ASTStatistic::evaluate(program);
			LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

			// Creates dot graph of the generated IR
			if(!CommandLineOptions::ShowIR.empty()) {
				insieme::utils::Timer timer("Show.graph");
				std::fstream dotFile(CommandLineOptions::ShowIR.c_str(), std::fstream::out | std::fstream::trunc);
				insieme::driver::printDotGraph(program, errors, dotFile);
				timer.stop();
				DLOG(INFO) << timer;
			}

			// XML dump
			if(!CommandLineOptions::DumpXML.empty()) {
				LOG(INFO) << "================================== XML DUMP =====================================";
				insieme::utils::Timer timer("Xml.dump");
				xml::XmlUtil::write(program, CommandLineOptions::DumpXML);
				timer.stop();
				LOG(INFO) << timer;
				LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
			}
		}
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

		if(CommandLineOptions::PrettyPrint || !CommandLineOptions::DumpIR.empty()) {
			// a pretty print of the AST
			LOG(INFO) << "========================= Pretty Print INSPIRE ==================================";
			if(!CommandLineOptions::DumpIR.empty()) {
				// write into the file
				std::fstream fout(CommandLineOptions::DumpIR,  std::fstream::out | std::fstream::trunc);
				fout << "// -------------- Pretty Print Inspire --------------" << std::endl;
				fout << insieme::core::printer::PrettyPrinter(program);
				fout << std::endl << std::endl << std::endl;
				fout << "// --------- Pretty Print Inspire - Detail ----------" << std::endl;
				fout << insieme::core::printer::PrettyPrinter(program, insieme::core::printer::PrettyPrinter::OPTIONS_DETAIL);
			} else
				LOG(INFO) << insieme::core::printer::PrettyPrinter(program);
			// LOG(INFO) << "====================== Pretty Print INSPIRE Detail ==============================";
			// LOG(INFO) << insieme::core::printer::PrettyPrinter(program, insieme::core::printer::PrettyPrinter::OPTIONS_DETAIL);
			// LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
		}

		if (CommandLineOptions::OpenCL) {
			/*LOG(INFO) << "Converting to OpenCL ... ";

			insieme::opencl_backend::OpenCLChecker oc;
			LOG(INFO) << "Checking OpenCL compatibility ... " << (oc.check(program) ? "CHECKED" : "WRONG");

			insieme::opencl_backend::ConversionContext cc;
			auto converted = cc.convert(program);
			// TODO write to output file
			std::cout << converted;*/
		} else {
			insieme::utils::Timer timer("Simple.Backend");

			LOG(INFO) << "========================== Converting to C++ ================================";
			insieme::simple_backend::ConversionContext cc(program);
			auto converted = cc.convert(program);

			if(!CommandLineOptions::Output.empty()) {
				insieme::backend::Rewriter::writeBack(program, CommandLineOptions::Output);
			} else
				LOG(INFO) << converted;

			timer.stop();
			LOG(INFO) << timer;
		}

	} catch (fe::ClangParsingError& e) {
		cerr << "Error while parsing input file: " << e.what() << endl;
	}

	ShutdownGoogleLogging();
}

