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

#include "expressions.h"
#include "types.h"
#include "expressions.h"
#include "statements.h"
#include "backend_convert.h"
#include "opencl_convert.h"
#include "naming.h"

#include "ast_statistic.h"

#include "checks/typechecks.h"
#include "printer/pretty_printer.h"

#include "container_utils.h"
#include "string_utils.h"
#include "cmd_line_utils.h"

#include "clang_compiler.h"
#include "logging.h"

#include "xml_utils.h"

#include "dot_printer.h"
#include <fstream>

using namespace std;
using namespace google;
namespace fe = insieme::frontend;
namespace core = insieme::core;

int main(int argc, char** argv) {

	CommandLineOptions::Parse(argc, argv);
	insieme::utils::InitLogger(argv[0], INFO, true);

	LOG(INFO) << "Insieme compiler";

	core::SharedNodeManager manager = std::make_shared<core::NodeManager>();
	core::ProgramPtr program = core::Program::create(*manager);
	try {
		auto inputFiles = CommandLineOptions::InputFiles;
		LOG(INFO) << "Parsing input files: ";
		std::copy(inputFiles.begin(), inputFiles.end(), std::ostream_iterator<std::string>( std::cout, ", " ) );
		fe::Program p(manager);
		p.addTranslationUnits(inputFiles);

		std::cerr << "*** CALL GRAPH ***\n";
		p.dumpCallGraph();

		// do the actual clang to IR conversion
		insieme::core::ProgramPtr program = p.convert();
		LOG(INFO) << "Parsed Program: " << std::endl << *program;

		// perform checks
		MessageList errors = check(program, insieme::core::checks::getFullCheck());
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const Message& cur) {
			LOG(INFO) << cur << std::endl;
		});

		// IR statistiscs
		ASTStatistic stats = ASTStatistic::evaluate(program);
		LOG(INFO) << "Number of Shared Nodes: " << stats.getNumSharedNodes();
		LOG(INFO) << "Number of Addressable Nodes: " << stats.getNumAddressableNodes();
		LOG(INFO) << "Share Ratio: " << stats.getShareRatio();
		LOG(INFO) << "Height of tree: " << stats.getHeight();

		// a pretty print of the AST
		LOG(INFO) << "========================= Pretty Print INSPIRE ==================================";
		LOG(INFO) << insieme::core::printer::PrettyPrint(program);
		LOG(INFO) << "====================== Pretty Print INSPIRE Detail ==============================";
		LOG(INFO) << insieme::core::printer::PrettyPrint(program, false, false, false);
		LOG(INFO) << "================================= END ===========================================";

		std::fstream dotFile("inspire.dot", std::fstream::out | std::fstream::trunc);
		insieme::driver::printDotGraph(program, errors, dotFile);
		dotFile.close();

		// XML dump
		insieme::xml::xmlWrite(program, "insieme.xml");

		LOG(INFO) << "Has name annotation: " << ((program->hasAnnotation(insieme::c_info::CNameAnnotation::KEY)?"true":"false"));

		if (CommandLineOptions::OpenCL) {
			LOG(INFO) << "Converting to OpenCL ... ";
			insieme::opencl_backend::ConversionContext cc;
			auto converted = cc.convert(program);
			// TODO write to output file 
			std::cout << converted;
		} else {
			LOG(INFO) << "Converting to C++ ... ";
			insieme::simple_backend::ConversionContext cc;
			auto converted = cc.convert(program);
			// TODO write to output file 
			std::cout << converted;
		}

	} catch (fe::ClangParsingError& e) {
		cerr << "Error while parsing input file: " << e.what() << endl;
	}

	ShutdownGoogleLogging();
}

