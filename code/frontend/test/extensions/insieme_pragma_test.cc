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
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/frontend/translation_unit.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/convert.h"
#include "insieme/utils/config.h"
#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/transform.h"

#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/tu/ir_translation_unit_io.h"
#include "insieme/frontend/extensions/omp_frontend_plugin.h"
#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/utils/test/test_utils.h"
#include "insieme/utils/string_utils.h"

#include "../test_utils.inc"

using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace insieme::core;

unsigned actualCount = 0;

template<typename T>
std::function<void (const NodePtr&)> getCheckingLambda() {
	return makeLambdaVisitor([&](const NodePtr& node) {
		if(node->hasAnnotation(T::KEY))
			actualCount++;
	});
}

TEST(PragmaMatcherTest, checkPragmas) {
	NodeManager manager;

	ConversionSetup setup;
	setup.frontendPluginInit();

	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/insieme_pragmas.c", setup);

	EXPECT_EQ(17, tu.getPragmaList().size());
//	for(const auto& e : tu.getPragmaList())
//		std::cout << e->getType() << "\n";
}

TEST(PragmaMatcherTest, checkAnnotations) {
	NodeManager manager;

	const auto& tu = ConversionJob(CLANG_SRC_DIR "/inputs/insieme_pragmas.c").toIRTranslationUnit(manager);
	const auto& ir = insieme::frontend::tu::toIR(tu.getNodeManager(), tu);

	actualCount = 0;
	visitDepthFirst(ir, getCheckingLambda<insieme::annotations::DataRangeAnnotation>());
	EXPECT_EQ(1, actualCount);

	actualCount = 0;
	visitDepthFirst(ir, getCheckingLambda<insieme::annotations::DataTransformAnnotation>());
	EXPECT_EQ(1, actualCount);

	actualCount = 0;
	visitDepthFirst(ir, getCheckingLambda<insieme::annotations::TransformAnnotation>());
	EXPECT_EQ(11, actualCount);
}

TEST(PragmaMatcherTest, checkMark) {
	NodeManager manager;
	const ProgramPtr program = ConversionJob(CLANG_SRC_DIR "/inputs/insieme_pragmas.c").execute(manager);
	auto& entryPoints = program->getEntryPoints();
	EXPECT_EQ(2, entryPoints.size());

	EXPECT_EQ("muha", insieme::core::annotations::getAttachedName(entryPoints[0]));
	EXPECT_EQ("main", insieme::core::annotations::getAttachedName(entryPoints[1]));
}


