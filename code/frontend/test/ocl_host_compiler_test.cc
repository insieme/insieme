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
#include "insieme/core/checks/ir_checks.h"

//#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/data_annotations.h"


#include "insieme/frontend/program.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/utils/logging.h"

namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace annot = insieme::annotations;
using namespace insieme::utils::set;
using namespace insieme::utils::log;

TEST(OclHostCompilerTest, HelloHostTest) {
	Logger::get(std::cerr, DEBUG);
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "inputs");
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR));
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "../../../test/ocl/common/"); // lib_icl

//	CommandLineOptions::IncludePaths.push_back("/home/klaus/NVIDIA_GPU_Computing_SDK/shared/inc");
//	CommandLineOptions::IncludePaths.push_back("/home/klaus/NVIDIA_GPU_Computing_SDK/OpenCL/common/inc");

	CommandLineOptions::Defs.push_back("INSIEME");
	//    string kernelSrc = SRC_DIR + "../../frontend/test/hello.cl" + string(SRC_DIR) + "";
	//    CommandLineOptions::Defs.push_back("KERNEL=\"/home/klaus/insieme/code/frontend/test/hello.cl\"");

	CommandLineOptions::Verbosity = 2;
	core::NodeManager manager;
	core::ProgramPtr program;

	LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "inputs/hello_host.c" << "' to IR...";
	fe::Program prog(manager);

	prog.addTranslationUnit(std::string(SRC_DIR) + "inputs/hello_host.c");
	program = prog.convert();

	EXPECT_EQ(&program->getNodeManager(), &manager);
	EXPECT_TRUE(manager.contains(program));

	LOG(INFO) << "Done.";

	LOG(INFO) << "Starting OpenCL host code transformations";
	fe::ocl::HostCompiler hc(program);
	hc.compile();

	core::printer::PrettyPrinter pp(program, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Printing the IR: " << pp;

	auto semantic = core::check(program, insieme::core::checks::getFullCheck());
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const core::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size());
	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::Message& cur) {
		LOG(INFO) << cur << std::endl;
		/*        core::NodeAddress address = cur.getAddress();
		 core::NodePtr context = address.getParentNode(address.getDepth()-1);
		 std::cout << "\t Context: " <<
		 insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 3) << std::endl;
		 */
	});

	// check for the kernel's datarange pragma
	size_t cnt = 0;
	auto lookForAnnot = core::makeLambdaVisitor([&](const core::NodePtr& node) {
		if(node->hasAnnotation(annot::DataRangeAnnotation::KEY)) {
			++cnt;
//			std::cout << node << std::endl << *node->getAnnotation(annot::DataRangeAnnotation::KEY) << std::endl;
		}
	});

	visitDepthFirstOnce(program, lookForAnnot);

	EXPECT_EQ(1u, cnt);

}

TEST(OclHostCompilerTest, VecAddTest) {
	Logger::get(std::cerr, DEBUG);
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "inputs");
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR));
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "../../backend/test/ocl_kernel");
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "../../../test/ocl/common/"); // lib_icl

	CommandLineOptions::Defs.push_back("INSIEME");

	CommandLineOptions::Verbosity = 2;
	core::NodeManager manager;
	core::ProgramPtr program = core::Program::get(manager);

	LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "../../backend/test/ocl_kernel/vec_add.c" << "' to IR...";
	fe::Program prog(manager);

	prog.addTranslationUnit(std::string(SRC_DIR) + "../../backend/test/ocl_kernel/vec_add.c");
	program = prog.convert();
	LOG(INFO) << "Done.";

	LOG(INFO) << "Starting OpenCL host code transformations";
	fe::ocl::HostCompiler hc(program);
	hc.compile();

	core::printer::PrettyPrinter pp(program, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Printing the IR: " << pp;
	//    LOG(INFO) << pp;

	auto errors = core::check(program, insieme::core::checks::getFullCheck()).getErrors();
	EXPECT_EQ(0u, errors.size());
	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::Message& cur) {
		LOG(INFO) << cur << std::endl;
		/*        core::NodeAddress address = cur.getAddress();
		 core::NodePtr context = address.getParentNode(address.getDepth()-1);
		 std::cout << "\t Context: " <<
		 insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 3) << std::endl;
		 */
	});

}
