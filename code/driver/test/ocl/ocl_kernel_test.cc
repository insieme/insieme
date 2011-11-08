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
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/program.h"
#include "insieme/driver/driver_config.h"
#include "insieme/opencl_backend/opencl_convert.h"

//#include <glog/logging.h>
#include "insieme/utils/logging.h"

#include <iostream>
#include <fstream>
#include <sstream>

namespace fe = insieme::frontend;
namespace core = insieme::core;
using namespace insieme::utils::set;
using namespace insieme::backend::ocl;
using namespace insieme::utils::log;

TEST(OclDriverTest, KernelTest) {

	Logger::get(std::cerr, INFO);

	CommandLineOptions::Defs.push_back("INSIEME");
	CommandLineOptions::IncludePaths.push_back(std::string(SRC_DIR) + "../../frontend/test/inputs");


	core::NodeManager manager;
	core::ProgramPtr program = core::Program::create(manager);

	LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "ocl/test_kernels.cl" << "' to IR...";
	fe::Program prog(manager);
	prog.addTranslationUnit(std::string(SRC_DIR) + "ocl/test_kernels.cl");
	program = prog.convert();
	LOG(INFO) << "Done.";

	LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

	LOG(INFO) << "Converting IR to OpenCL...";
	auto converted = OpenCLBackend::getDefault()->convert(program);
	LOG(INFO) << "Printing converted code: " << *converted;

	std::ofstream out( std::string(SRC_DIR) + "/test_kernels.insieme.cl" );
	out << *converted;
	out.close();

	LOG(INFO) << "Wrote source to " << SRC_DIR << "/test_kernels.insieme.cl" << std::endl;
}
