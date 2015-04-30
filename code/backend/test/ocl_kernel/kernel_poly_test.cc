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

#include <vector>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/utils/logging.h"

#include "insieme/annotations/data_annotations.h"

#include "insieme/frontend/frontend.h"
#include "insieme/utils/config.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/backend/ocl_kernel/kernel_poly.h"
#include "insieme/utils/config.h"

#include "insieme/driver/cmd/insiemecc_options.h"

using namespace insieme::core;
using namespace insieme::driver;
using namespace insieme::core::lang;
using namespace insieme::core::annotations;
using namespace insieme::utils::set;
using namespace insieme::utils::log;

TEST(KernelPoly, RangeTest) {
	NodeManager manager;
	Logger::get(std::cerr, INFO);

	// Frontend PATH
	std::string includeA = "-I" CLANG_SRC_DIR;
	std::string includeB = "-I" CLANG_SRC_DIR "inputs";
	std::string includeC = "-I" CLANG_SRC_DIR "../../../test/ocl/common/";
	std::string includeD = "-I" OCL_KERNEL_TEST_DIR;
	std::string fileName = OCL_KERNEL_TEST_DIR "vec_add.c";
    std::vector<std::string> argv = { "compiler",  fileName, includeA, includeB, includeC, includeD, "-flib-icl"};
    cmd::Options options = cmd::Options::parse(argv);

	LOG(INFO) << "Converting input program '" << string(OCL_KERNEL_TEST_DIR) << "vec_add.c" << "' to IR...\n";

	// 	prog.addTranslationUnit(std::string(SRC_DIR) + "/inputs/hello_host.c"); // Other Input test :)
	//prog.addTranslationUnit(std::string(OCL_KERNEL_TEST_DIR) + "kernel.cl");
	ProgramPtr program = options.job.execute(manager);

	LOG(INFO) << "Start OpenCL analysis\n";

	insieme::backend::ocl_kernel::KernelPreprocessor kp;
	NodePtr newProg = (kp.process(manager, program[0]));

	EXPECT_TRUE(!!newProg);
	insieme::backend::ocl_kernel::KernelPoly polyAnalyzer(newProg);

	EXPECT_EQ(1u, polyAnalyzer.getKernels().size());
//	insieme::core::printer::PrettyPrinter pp(polyAnalyzer.getKernels().at(0));
//	std::cout << "Printing the IR: " << pp;

	size_t annotCnt = 0;
	auto searchRangeAnnot = makeLambdaVisitor([&](const NodePtr& node) {
		if(node->getNodeType() == insieme::core::NT_LambdaExpr) {
			if(node->hasAnnotation(insieme::annotations::DataRangeAnnotation::KEY)){
				++annotCnt;
				insieme::annotations::DataRangeAnnotationPtr dra = node->getAnnotation(insieme::annotations::DataRangeAnnotation::KEY);
				EXPECT_EQ(3u, dra->getRanges().size());

				for_each(dra->getRanges(), [&](insieme::annotations::Range range) {
					EXPECT_TRUE(toString(range).find("get_global_id") != string::npos)  << range;
					int readCnt = 0, writeCnt = 0;
					switch(range.getAccessType()) {
						case insieme::ACCESS_TYPE::read: ++readCnt; break;
						case insieme::ACCESS_TYPE::write: ++writeCnt; break;
						default: EXPECT_FALSE(true); break;
					}
				});

				EXPECT_TRUE(toString(*dra).find("get_global_id") != string::npos);
//				std::cout << *dra << std::endl;
			}
		}
	});

	visitDepthFirstOnce(newProg, searchRangeAnnot);

	EXPECT_EQ(1u, annotCnt);
}
