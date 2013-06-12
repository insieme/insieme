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

#include "insieme/frontend/program.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/annotations/c/naming.h"

#include "insieme/frontend/clang_config.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/parser2/ir_parser.h"

#include "insieme/core/analysis/normalize.h"

#include "insieme/utils/logging.h"

namespace fe = insieme::frontend;
namespace core = insieme::core;
using namespace insieme::utils::set;
using namespace insieme::utils::log;
using namespace core;

LambdaExprPtr getEntryPoint(const core::ProgramPtr& prog, const std::string& entry)
{
	for(auto it=prog->getEntryPoints().begin(); it!=prog->getEntryPoints().end(); it++)
	{
		LambdaExprPtr fun = static_pointer_cast<LambdaExprPtr>(*it);
		std::stringstream ss;
		ss << *(fun->getAnnotations().begin())->second;

		if(ss.str().compare(entry) == 0) {
			return analysis::normalize(fun);
		}
	}

	return LambdaExprPtr();
}

TEST(OMPx, SimpleRegion) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// C source code compilation

    fe::ConversionJob job(SRC_DIR "inputs/omp+_region.c");
    job.addIncludeDirectory(SRC_DIR "inputs");
    job.setOption(fe::ConversionJob::OpenMP);

    LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

    core::ProgramPtr prog = job.execute(manager);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "simpleRegion");
	ASSERT_TRUE(progEntry);

    LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(progEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

    // Target IR code

    LOG(INFO) << "Parsing reference IR code...";

	// Main function with region

	auto res = analysis::normalize(builder.parseProgram(
			"let fun000 = ()->unit {"
				"{"
					"ref<int<4>> v1 = var(3);"
				"};"
			"};"

    		"int<4> main() {"
				"ref<int<4>> v1 = var(0);"
				"ref<int<4>> v5 = var(0);"
				"{"
					"merge(parallel(job([1-1], fun000())));"
				"};"
				"return 0;"
    		"}"));
	ASSERT_TRUE(res);
	auto resEntry = *(res->getEntryPoints().begin());
	ASSERT_TRUE(resEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(resEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

    LOG(INFO) << "Comparing results...";

	// print program using pretty printer
    EXPECT_EQ(toString(core::printer::PrettyPrinter(progEntry)), toString(core::printer::PrettyPrinter(resEntry)));
}

TEST(OMPx, FirstLocal) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// C source code compilation

    fe::ConversionJob job(SRC_DIR "inputs/omp+_region.c");
    job.addIncludeDirectory(SRC_DIR "inputs");
    job.setOption(fe::ConversionJob::OpenMP);

    LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

    core::ProgramPtr prog = job.execute(manager);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "firstLocal");
	ASSERT_TRUE(progEntry);

    LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(progEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

    // Target IR code

    LOG(INFO) << "Parsing reference IR code...";

	auto res = analysis::normalize(builder.parseProgram(
			"let fun000 = (int<4> v0)->unit {"
    			"ref<int<4>> v2 = loc(v0);"
				"{"
					"ref<int<4>> v3 = var(*v2 + 3);"
				"};"
			"};"

    		"int<4> main() {"
				"ref<int<4>> v1 = var(5);"
				"{"
					"int<4> v2 = *v1;"
					"merge(parallel(job([1-1], fun000(v2))));"
				"};"
				"return 0;"
    		"}"));
	ASSERT_TRUE(res);
	auto resEntry = *(res->getEntryPoints().begin());
	ASSERT_TRUE(resEntry);

	LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(resEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

    LOG(INFO) << "Comparing results...";

	// print program using pretty printer
    EXPECT_EQ(toString(core::printer::PrettyPrinter(progEntry)), toString(core::printer::PrettyPrinter(resEntry)));
}
