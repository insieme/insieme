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

#include <gtest/gtest.h>

#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/transform.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/compiler.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/extensions/omp_frontend_extension.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/translation_unit.h"
#include "insieme/frontend/tu/ir_translation_unit_io.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/config.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/test/test_utils.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "../test_utils.inc"

using namespace insieme::frontend;
using namespace insieme::driver;
using namespace insieme::frontend::pragma;
using namespace insieme::core;
using namespace insieme::core::pattern;

unsigned actualCount = 0;

template <typename T>
std::function<void(const NodePtr&)> getCheckingLambda() {
	return makeLambdaVisitor([&](const NodePtr& node) {
		if(node->hasAnnotation(T::KEY)) { actualCount++; }
	});
}

TEST(InsiemePragmaTest, checkPragmas) {
	NodeManager manager;

	const std::string fileName = FRONTEND_TEST_DIR "/inputs/insieme_pragmas.c";
	std::vector<std::string> argv = {"compiler", fileName};
	cmd::Options options = cmd::Options::parse(argv);
	options.job.frontendExtensionInit();

	insieme::frontend::TranslationUnit tu(manager, fileName, options.job);

	EXPECT_EQ(4, tu.getPragmaList().size());
	//	for(const auto& e : tu.getPragmaList())
	//		std::cout << e->getType() << "\n";
}

TEST(InsiemePragmaTest, checkMark) {
	NodeManager manager;
	const std::string fileName = FRONTEND_TEST_DIR "/inputs/insieme_pragmas.c";
	std::vector<std::string> argv = {"compiler", fileName};
	cmd::Options options = cmd::Options::parse(argv);

	const ProgramPtr program = options.job.execute(manager);
	EXPECT_TRUE(program);
	auto& entryPoints = program->getEntryPoints();
	EXPECT_EQ(2, entryPoints.size());

	EXPECT_EQ("muha", insieme::core::annotations::getAttachedName(entryPoints[0]));
	EXPECT_EQ("main", insieme::core::annotations::getAttachedName(entryPoints[1]));
}


TEST(InsiemePragmaTest, checkTransformations) {
	NodeManager manager;
	const std::string fileName = FRONTEND_TEST_DIR "/inputs/insieme_pragmas.c";
	std::vector<std::string> argv = {"compiler", fileName};
	cmd::Options options = cmd::Options::parse(argv);

	const auto& tu = options.job.toIRTranslationUnit(manager);
	const auto& ir = insieme::frontend::tu::toIR(tu.getNodeManager(), tu);
	EXPECT_TRUE(ir);

	actualCount = 0;
	visitDepthFirst(ir, getCheckingLambda<insieme::annotations::TransformAnnotation>());
	EXPECT_EQ(1, actualCount);
}

TEST(InsiemePragmaTest, checkFunctionUnrolling) {
	namespace icp = insieme::core::pattern;
	NodeManager manager;

	Source src(
	    R"(
#pragma insieme fun_unroll (3)
int muha(int i) {
if (i == 0) return 0;
return muha(i-1)+2;
}
int main() {
					int arrayA[10];

					muha(42);
					return 0;
				}
			)");
	const boost::filesystem::path& fileName = src;
	std::vector<std::string> argv = {"compiler", fileName.string()};
	cmd::Options options = cmd::Options::parse(argv);

	const ProgramPtr program = options.job.execute(manager);
	EXPECT_TRUE(program);

	// check for four function calls
	TreePattern pattern = irp::callExpr(icp::aT(irp::callExpr(icp::aT(irp::callExpr(icp::aT(irp::callExpr(icp::any)))))));

	auto res = irp::collectAll(pattern, program, false);
	ASSERT_EQ(1, res.size());
	EXPECT_TRUE(res.front());
}

TEST(InsiemePragmaTest, checkReschedule) {
	namespace icp = insieme::core::pattern;
	NodeManager manager;

	Source src(
	    R"(int main() {
					int arrayA[10][10];
					#pragma insieme reschedule (0)
					for(int i = 0; i < 10; ++i) {
						for(int j = 0; j < 10; ++j) {
							arrayA[i][j] = 0;
						}
					}
					return 0;
				}
			)");

	const boost::filesystem::path& fileName = src;
	std::vector<std::string> argv = {"compiler", fileName.string()};
	cmd::Options options = cmd::Options::parse(argv);

	const ProgramPtr program = options.job.execute(manager);
	EXPECT_TRUE(program);

	auto at = [&manager](const string& str) { return irp::atom(manager, str); };
	// check for identity
	TreePattern pattern = irp::forStmt(icp::any, at("0"), icp::any, at("1"), irp::forStmt(icp::any, at("0"), icp::any, at("1"), irp::assignment()));

	auto res = irp::collectAll(pattern, program, false);
	ASSERT_EQ(1, res.size());
	EXPECT_TRUE(res.front());
}
