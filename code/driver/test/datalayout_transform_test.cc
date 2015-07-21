/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include <fstream>
#include <sstream>
#include <cmath>
#include <vector>
#include <string>

#include <gtest/gtest.h>

#include "insieme/core/ir_program.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/frontend/frontend.h"

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"

#include "insieme/transform/datalayout/aos_to_soa.h"

#include "insieme/driver/cmd/insiemecc_options.h"

using namespace insieme;

TEST(DatalayoutTransformTest, OclTest) {
	core::NodeManager manager;

	LOG(INFO) << "Converting input program '" << std::string(SRC_ROOT_DIR) << "transform/test/datalayout/inputs/sparsevec.c" << "' to IR...";
    std::string inputFile = SRC_ROOT_DIR "transform/test/datalayout/inputs/sparsevec.c";
    std::string includeA = "-I" SRC_ROOT_DIR "transform/test/datalayout/inputs/";
    std::string includeB = "-I" CLANG_SRC_DIR "inputs";
    std::vector<std::string> args = {"compiler", inputFile, includeA, includeB, "-fopencl"};
    driver::cmd::Options options = driver::cmd::Options::parse(args);
	options.job.setDefinition("INSIEME", "");
	options.job.setDefinition("UNIX", "");


	core::ProgramPtr program = options.job.execute(manager, false);
	LOG(INFO) << "Done.";

	core::NodePtr prog = program->getElement(0);

//	transform::datalayout::AosToSoa ats(prog);
//	ats.transform();

//	dumpPretty(prog);

	auto errors = core::checks::check(prog).getAll();

	EXPECT_EQ(errors.size(), 0u);

	std::sort(errors.begin(), errors.end());

	for_each(errors, [](const core::checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});


}
