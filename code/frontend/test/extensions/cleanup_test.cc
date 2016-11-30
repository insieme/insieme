/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/frontend/extensions/frontend_cleanup_extension.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/tu/ir_translation_unit_io.h"

namespace insieme {
namespace frontend {

	TEST(CleanupExtension, Assignment) {

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		auto& feExt = mgr.getLangExtension<utils::FrontendInspireModule>();
		// add FE module symbols for use in test cases
		auto symbols = mgr.getLangExtension<frontend::utils::FrontendInspireModule>().getSymbols();

		core::ExpressionPtr prog = builder.parseExpr(R"(
			def main = () -> int<4> {
				var ref<int<4>> v0;
				var ref<int<4>> v1;
				c_style_assignment(v0, 5);
				c_style_assignment(v0, c_style_assignment(v1, 1));

				if(true) {
					c_style_assignment(v0, *v1 - 2);
					c_style_assignment(v1, *v1 - *v0);
				}

				cxx_style_assignment(v0, 2);
				return 0;
			};
			main
		)", symbols);

		auto res = core::checks::check(prog);
		ASSERT_TRUE(res.empty()) << res;

		auto lit = builder.literal(builder.stringValue("main"), prog->getType());
		core::tu::IRTranslationUnit tu(mgr);
		tu.addFunction(lit, prog.as<core::LambdaExprPtr>());
		tu.addEntryPoints(lit);

		extensions::FrontendCleanupExtension cleanup;
		tu = cleanup.IRVisit(tu);

		prog = tu.resolve(lit).as<core::ExpressionPtr>();
		res = core::checks::check(prog);
		ASSERT_TRUE(res.empty()) << "Semantic error after cleanup:\n" << res;

		// check that only assignments for which the return value is used (that is, the parent is an expression or declaration) remain
		visitDepthFirst(core::ExpressionAddress(prog), [&](const core::CallExprAddress& call) {
			if(feExt.isCallOfCStyleAssignment(call) || feExt.isCallOfCxxStyleAssignment(call)) {
				auto parentCategory = call.getParentNode().getNodeCategory();
				if(call.getParentNode().getNodeType() == core::NT_Declaration && call.getDepth() >= 2) parentCategory = call.getParentNode(2).getNodeCategory();
				EXPECT_EQ(core::NC_Expression, parentCategory);
			}
		});

		// check that there is still the inner c style assignment
		EXPECT_EQ(1, core::analysis::countInstances(prog, feExt.getCStyleAssignment(), false));
		// .. and no cxx style assignments are left
		EXPECT_EQ(0, core::analysis::countInstances(prog, feExt.getCxxStyleAssignment(), false));
	}

} // fe namespace
} // insieme namespace
