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

#include "insieme/core/expressions.h"
#include "insieme/core/types.h"
#include "insieme/core/expressions.h"
#include "insieme/core/statements.h"
#include "insieme/core/ast_statistic.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/simple_backend/backend_convert.h"
#include "insieme/simple_backend/rewrite.h"

#include "insieme/opencl_backend/opencl_convert.h"
#include "insieme/opencl_backend/opencl_checker.h"

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

#include "insieme/frontend/clang_compiler.h"

#include "insieme/driver/dot_printer.h"

#include "insieme/xml/xml_utils.h"

#include <fstream>

using namespace std;
using namespace google;
namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace xml = insieme::xml;

int main(int argc, char** argv) {

	CommandLineOptions::Parse(argc, argv);
	insieme::utils::InitLogger(argv[0], INFO, true);

	LOG(INFO) << "Insieme compiler";

	core::SharedNodeManager manager = std::make_shared<core::NodeManager>();
	core::ProgramPtr program = core::Program::create(*manager);
	try {
		if(!CommandLineOptions::InputFiles.empty()) {
			auto inputFiles = CommandLineOptions::InputFiles;
			LOG(INFO) << "Parsing input files: ";
			std::copy(inputFiles.begin(), inputFiles.end(), std::ostream_iterator<std::string>( std::cout, ", " ) );
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
			LOG(INFO) << "=========================== IR Semantic Checks ==================================";
			insieme::utils::Timer timer("Checks");
			MessageList&& errors = check(program, insieme::core::checks::getFullCheck());
			std::sort(errors.begin(), errors.end());
			for_each(errors, [](const Message& cur) {
				LOG(INFO) << cur << std::endl;
			});
			timer.stop();
			LOG(INFO) << timer;
			LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

			// IR statistiscs
			ASTStatistic&& stats = ASTStatistic::evaluate(program);
			LOG(INFO) << "============================ IR Statistics ======================================";
			LOG(INFO) << "Number of Shared Nodes: " << stats.getNumSharedNodes();
			LOG(INFO) << "Number of Addressable Nodes: " << stats.getNumAddressableNodes();
			LOG(INFO) << "Share Ratio: " << stats.getShareRatio();
			LOG(INFO) << "Height of tree: " << stats.getHeight();
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
			NodePtr&& xmlNode= xml::XmlUtil::read(*manager, CommandLineOptions::LoadXML);
			// used for debuging XML, removed once in production
			if(program) {
				assert(xmlNode != program);
				assert(*xmlNode == *program);
				assert(equalsWithAnnotations(xmlNode, program));
			}
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
				fout << insieme::core::printer::PrettyPrint(program);
			} else
				LOG(INFO) << insieme::core::printer::PrettyPrint(program);
			LOG(INFO) << "====================== Pretty Print INSPIRE Detail ==============================";
			LOG(INFO) << insieme::core::printer::PrettyPrint(program, false, false, false);
			LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
		}

		LOG(INFO) << "Has name annotation: " << ((program->hasAnnotation(insieme::c_info::CNameAnnotation::KEY)?"true":"false"));

		if (CommandLineOptions::OpenCL) {
			LOG(INFO) << "Converting to OpenCL ... ";

			insieme::opencl_backend::OpenCLChecker oc;
			LOG(INFO) << "Checking OpenCL compatibility ... " << (oc.check(program) ? "CHECKED" : "WRONG");

			insieme::opencl_backend::ConversionContext cc;
			auto converted = cc.convert(program);
			// TODO write to output file
			std::cout << converted;
		} else {
			insieme::utils::Timer timer("Simple.Backend");

			LOG(INFO) << "========================== Converting to C++ ================================";
			insieme::simple_backend::ConversionContext cc;
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

