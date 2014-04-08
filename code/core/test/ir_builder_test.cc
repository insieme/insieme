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

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {

TEST(BuilderTest, CreateCallExprFromBody) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = mgr.getLangBasic();

	DeclarationStmtPtr a = builder.declarationStmt(builder.variable(gen.getInt4()));

//	std::map<string,core::NodePtr> symbols;
//	symbols["a"] = a;
//	StatementPtr body = builder.parseStmt(
//			"{"
//			"a = 7;"
//			"}"
//	);

	StatementPtr stmt = builder.parseStmt(
		"{"
			"let fun = (int<4> arg)->int<4> { return arg + 1; };"
			"let rfun = (ref<int<4>> arg)->int<4> { return *arg + 1; };"
			"ref<int<4>> a;"
			"ref<int<4>> b;"
			"ref<int<4>> c;"
			"ref<int<4>> d;"
			"ref<int<4>> e;"
			"ref<int<4>> f;"
			"ref<int<4>> g;"
			"{"
				"a = 7;"
				"fun(*b);"
				"rfun(c);"
				"fun(fun(*d));"
				"fun(rfun(e));"
				"rfun(var(rfun(f)));"
				"rfun(var(fun(*g)));"
			"}"
		"}"
	);

	StatementPtr body;
	visitDepthFirstPrunable(stmt, [&](const CompoundStmtPtr& cs) {
		if(cs->getChildList().size() == 7) {
			body = cs;
			return true;
		}
		return false;
	});

	NodePtr call = builder.createCallExprFromBody(body, gen.getUnit());
	NodePtr embeddedCall = transform::replaceAll(mgr, stmt, body, call);

	dumpPretty(embeddedCall);

	auto semantic = core::checks::check(embeddedCall);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const core::checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
//	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::checks::Message& cur) {
		std::cout << cur << std::endl;
	});

}

}
}
