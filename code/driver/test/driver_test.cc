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

#include <gtest/gtest.h>

#include "insieme/core/ir_program.h"

#include "insieme/frontend/frontend.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/driver/driver_config.h"
#include "insieme/core/printer/pretty_printer.h"

//#include <glog/logging.h>
#include "insieme/utils/logging.h"

#include <iostream>
#include <fstream>
#include <sstream>

namespace fe = insieme::frontend;
namespace core = insieme::core;
using namespace insieme::utils::set;
using namespace insieme::backend::sequential;
using namespace insieme::utils::log;

TEST(DriverTest, HelloWorldTest) {

	Logger::get(std::cerr, INFO);

	core::NodeManager manager;

	LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "/hello_world.c" << "' to IR...";
	fe::ConversionJob job(SRC_DIR "/hello_world.c");
	core::ProgramPtr program = job.execute(manager);
	LOG(INFO) << "Done.";

    LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

	LOG(INFO) << "Converting IR to C...";
	auto converted = SequentialBackend::getDefault()->convert(program);
	LOG(INFO) << "Printing converted code: " << *converted;

	std::ofstream out( std::string(SRC_DIR) + "/hello_world.insieme.c" );
	out << *converted;
	out.close();

	LOG(INFO) << "Wrote source to " << SRC_DIR << "/hello_world.insieme.c" << std::endl;
}

//TEST(DriverTest, MultiTUTest) {
//
//	// insieme::utils::InitLogger("MultiTUTest", INFO, true);
//
//	core::SharedNodeManager sharedManager = std::make_shared<core::NodeManager>();
//	core::ProgramPtr program = core::Program::create(*sharedManager);
//
//	fe::Program prog(sharedManager);
//	prog.addTranslationUnit(std::string(SRC_DIR) + "/multi_tu/b.c");
//	prog.addTranslationUnit(std::string(SRC_DIR) + "/multi_tu/a.c");
//	LOG(INFO) << "Start conversion...";
//	program = prog.convert();
//	LOG(INFO) << "Done.";
//
//	LOG(INFO) << "Printing the IR: " << program;
//	LOG(INFO) << "Converting IR to C...";
//
//	ConversionContext cc;
//	auto converted = cc.convert(program);
//
//	std::ostringstream ss;
//	for_each(program->getEntryPoints(), [&converted, &ss](const ExpressionPtr& ep) {
//		ss << converted[ep] << std::endl;
//	});
//
//	LOG(INFO) << "Printing converted code: " << ss.str();
//
//	std::ofstream out( std::string(SRC_DIR) + "/multi_tu.insieme.c" );
//	out << ss.str();
//	out.close();
//
//	LOG(INFO) << "Wrote source to " << SRC_DIR << "/multi_tu.insieme.c" << std::endl;
//}
