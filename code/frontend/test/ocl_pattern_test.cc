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

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

namespace core = insieme::core;
namespace fe = insieme::frontend;
namespace p = insieme::core::pattern;

TEST(OclHostCompilerTest, HelloHostTest) {
	Logger::get(std::cerr, ERROR, 0);

	core::NodeManager manager;

	// create and customize conversion job
	fe::ConversionJob job(CLANG_SRC_DIR  "inputs/ocl_functions.c");
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");
	job.addIncludeDirectory(CLANG_SRC_DIR);
	job.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/common/");

	core::ProgramPtr program = job.execute(manager);
	LOG(INFO) << "Done.";

	EXPECT_EQ(&program->getNodeManager(), &manager);
	EXPECT_TRUE(manager.contains(program));

	core::NodeAddress root(program->getElement(0));

	p::TreePatternPtr clSetKernelArg =  p::irp::callExpr(p::any, p::irp::literal("clSetKernelArg"), var("kernel", p::any) << var("idx", p::any) <<
			var("size", p::any) << var("arg", p::any));

	std::string kernelString = "ref<array<_cl_kernel,1>>";

//	dumpPretty(root);

	std::string variableName[8];
	variableName[0] = "kernel1";
	variableName[1] = "kernel2";
	variableName[2] = "kernel3";
	variableName[3] = "kernel4";
	variableName[4] = "v21";
	variableName[5] = "v32";
	variableName[6] = "v33";
	variableName[7] = "v34";

	int cnt = 0;
	p::irp::matchAllPairs(clSetKernelArg, root, [&](const core::NodeAddress& matchAddress, const p::AddressMatch& setArg) {
		core::ExpressionAddress kernel = setArg["kernel"].getValue().as<core::ExpressionAddress>();

		core::ExpressionPtr kernelRoot = fe::ocl::utils::getRootVariable(matchAddress >> kernel).as<core::ExpressionPtr>();

		core::TypePtr kernelType = kernelRoot->getType();

		EXPECT_NE(std::string::npos, kernelType->toString().find(kernelString));
		EXPECT_EQ(variableName[cnt], kernelRoot->toString());


		++cnt;
	});

	EXPECT_EQ(8, cnt);

}
